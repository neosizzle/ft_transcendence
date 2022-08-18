import {
  BadRequestException,
  Injectable,
  NotFoundException,
} from "@nestjs/common";
import { Prisma, User, UserStatus } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListObject, ListQuery, validateListquery } from "src/utils";
import { BlocksDto } from "./dto";

/**
 * Transforms string values into values of their specific type
 *
 * @param filterBy Filterby string
 * @param filterOn Filteron string
 * @returns
 */
const transformFilterUser = (
  filterBy: string,
  filterOn: string
): string | number | UserStatus | Date => {
  if (filterOn === "id" || filterOn === "level" || filterOn === "ranking" || filterOn === "wins" || filterOn === "losses")
    return parseFloat(filterBy) ? parseFloat(filterBy) : -1;
  if (filterOn === "status") {
    switch (filterBy) {
      case "LOGGEDIN":
        return UserStatus.LOGGEDIN;
      case "INGAME":
        return UserStatus.INGAME;
      case "OFFLINE":
        return UserStatus.OFFLINE;
      default:
        return undefined;
    }
  }
  if (filterOn === "createdAt" || filterOn === "updatedAt")
    return new Date(filterBy);
  else return filterBy;
};

const generateFilters = (listObj: ListObject): Prisma.BlockWhereInput => {
  const filterOptns: Prisma.BlockWhereInput = {};

  if (listObj.operator === "AND") {
    filterOptns.AND = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterUser(
        listObj.filterBys[index],
        listObj.filterOns[index]
      );
      const filterOn = listObj.filterOns[index];
      const filterEntry = {};
      if (filterOn == "reqStatus") continue;
      if (
        filterBy == "LOGGEDIN" ||
        filterBy == "INGAME" ||
        filterBy == "OFFLINE" ||
        typeof filterBy !== "string"
      )
        filterEntry[filterOn] = { equals: filterBy };
      else filterEntry[filterOn] = { contains: filterBy };
      filterOptns.AND.push({ blockee: filterEntry });
    }
  } else if (listObj.operator === "OR") {
    filterOptns.OR = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterUser(
        listObj.filterBys[index],
        listObj.filterOns[index]
      );
      const filterOn = listObj.filterOns[index];
      const filterEntry = {};
      if (filterOn == "reqStatus") continue;
      if (
        filterBy == "LOGGEDIN" ||
        filterBy == "INGAME" ||
        filterBy == "OFFLINE" ||
        typeof filterBy !== "string"
      )
        filterEntry[filterOn] = { equals: filterBy };
      else filterEntry[filterOn] = { contains: filterBy };
      filterOptns.OR.push({ blockee: filterEntry });
    }
  }
  return filterOptns;
};

/**
 * Populates the 'where' option with filter values
 * @param listObj Listobject
 * @returns Generates the sorting field for the query payload
 */
const generateBlocksWhere = (
  listObj: ListObject,
  userId: number
): Prisma.BlockWhereInput => {
  // specify only to search for users containing me
  const res: Prisma.BlockWhereInput = {};
  res.AND = [];
  res.AND.push({ blockerId: userId });
  res.AND.push(generateFilters(listObj));

  return res;
};

/**
 * Generates pagination settings
 * Generates filters
 * Generates sorts
 *
 * @param listObj List object
 * @returns Generated find query payload
 */
const generateBlockPayload = (
  listObj: ListObject,
  userId: number
): Prisma.BlockFindManyArgs => {
  const res: Prisma.BlockFindManyArgs = {};

  res.include = {
    blockee: true,
    blocker: true,
  };
  res.take = listObj.pageSize;
  res.skip = (listObj.page - 1) * listObj.pageSize;
  res.where = generateBlocksWhere(listObj, userId);
  if (listObj.sortBy) {
    res.orderBy = [{ blockee: {} }];
    res.orderBy[0].blockee[listObj.sortOn] =
      listObj.sortBy === "Ascending" ? "asc" : "desc";
  }
  return res;
};

@Injectable()
export class BlocksService {
  constructor(private prisma: PrismaService) {}

  // list blocks
  async getBlocks(user: User, query: ListQuery) {
    // validate query
    const listObj = validateListquery(query, Object.keys(user));
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateBlockPayload(listObj.data, user.id);
    const res = await this.prisma.block.findMany(payload);
    delete payload.include;
    delete payload.take;
    delete payload.skip;
    const total_elements = await this.prisma.block.count(
      <Prisma.BlockCountArgs>payload
    );

    return { data: res, total_elements };
  }

  // add block
  async addBlock(user: User, dto: BlocksDto) {
    // validate dto
    if (!dto.id && !dto.intraName)
      throw new BadRequestException("Need to provide intraName or id as body");

    if (dto.id && dto.intraName)
      throw new BadRequestException("only intra name or userid is needed");

    // check if user exist
    const payloadWhere = {};
    if (dto.id) payloadWhere["id"] = dto.id;
    else payloadWhere["intraName"] = dto.intraName;
    const userToBlock = await this.prisma.user.findFirst({
      where: payloadWhere,
    });

    if (!userToBlock) throw new NotFoundException("user not found");

    // check if user is self
    if (userToBlock.id == user.id)
      throw new BadRequestException("Unable to block yourself");

    // check if block existed
    const existingBlock = await this.prisma.block.count({
      where: {
        blockerId: user.id,
        blockeeId: userToBlock.id,
      },
    });
    if (existingBlock) throw new BadRequestException("Already blocked person");

    // create entry in block database
    const res = await this.prisma.block.create({
      data: {
        blockerId: user.id,
        blockeeId: userToBlock.id,
      },
    });

    return res;
  }

  // remove block ( unblock )
  async removeBlock(user: User, blockeeId: number) {
    // check if user trying to remove self
    if (user.id === blockeeId)
      throw new BadRequestException("Cannot remove self");

    // delete all block relations between userid and blockeeid
    const res = await this.prisma.block.deleteMany({
      where: {
        blockerId: user.id,
        blockeeId,
      },
    });

    return res;
  }
}
