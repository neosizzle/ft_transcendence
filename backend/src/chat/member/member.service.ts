import {
  BadRequestException,
  Injectable,
  InternalServerErrorException,
  NotFoundException,
  UnauthorizedException,
} from "@nestjs/common";
import { Member, Prisma, RoomType, User } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListObject, ListQuery, validateListquery } from "src/utils";
import { MemberDto } from "./dto";
import * as CryptoJS from "crypto-js";

/**
 * Transforms string values into values of their specific type
 *
 * @param filterBy Filterby string
 * @param filterOn Filteron string
 * @returns
 */
const transformFilterMember = (
  filterBy: string,
  filterOn: string
): string | number | Date => {
  if (filterOn === "createdAt" || filterOn === "updatedAt")
    return new Date(filterBy);
  if (filterOn === "id" || filterOn === "userId" || filterOn === "roomId")
    return parseInt(filterBy, 10);
  else return filterBy;
};

/**
 * Populates the 'where' option with filter values
 * @param listObj Listobject
 * @returns Generates the sorting field for the query payload
 */
const generateMemberWhere = (listObj: ListObject): Prisma.MemberWhereInput => {
  const res: Prisma.MemberWhereInput = {};

  if (listObj.operator === "AND") {
    res.AND = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterMember(
        listObj.filterBys[index],
        listObj.filterOns[index]
      );
      const filterOn = listObj.filterOns[index];
      const filterEntry = {};
      if (typeof filterBy !== "string")
        filterEntry[filterOn] = { equals: filterBy };
      else filterEntry[filterOn] = { contains: filterBy };
      res.AND.push(filterEntry);
    }
  } else if (listObj.operator === "OR") {
    res.OR = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterMember(
        listObj.filterBys[index],
        listObj.filterOns[index]
      );
      const filterOn = listObj.filterOns[index];
      const filterEntry = {};
      if (typeof filterBy !== "string")
        filterEntry[filterOn] = { equals: filterBy };
      else filterEntry[filterOn] = { contains: filterBy };
      res.OR.push(filterEntry);
    }
  }

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
const generateMemberPayload = (
  listObj: ListObject
): Prisma.MemberFindManyArgs => {
  const res: Prisma.MemberFindManyArgs = {};

  res.take = listObj.pageSize;
  res.skip = (listObj.page - 1) * listObj.pageSize;
  res.include = { user: true, room: true };
  if (listObj.filterBys) res.where = generateMemberWhere(listObj);
  if (listObj.sortBy) {
    res.orderBy = [{}];
    res.orderBy[0][listObj.sortOn] =
      listObj.sortBy === "Ascending" ? "asc" : "desc";
  }
  return res;
};

@Injectable()
export class MemberService {
  constructor(private prisma: PrismaService) {}

  private sampleMember: Member = {
    id: -1,
    userId: -1,
    roomId: -1,
  };

  // list members
  async getMembers(query: ListQuery) {
    const keys = Object.keys(this.sampleMember);

    // validate query
    const listObj = validateListquery(query, keys);
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateMemberPayload(listObj.data);
    const res = await this.prisma.member.findMany(payload);
    delete payload.skip;
    delete payload.take;
    delete payload.include;
    const total_elements = await this.prisma.member.count(
      <Prisma.MemberCountArgs>payload
    );

    return { data: res, total_elements };
  }

  // add member (join roon)
  async addMember(user: User, dto: MemberDto) {
    //verify room exists
    const roomToJoin = await this.prisma.room.findUnique({
      where: { id: dto.roomId },
    });

    if (!roomToJoin) throw new NotFoundException("Room not found");

    // verify password if room is protected
    if (roomToJoin.isProtected) {
      if (CryptoJS.SHA256(dto.password).toString() != roomToJoin.password)
        throw new UnauthorizedException("Bad password");
    }

    // check for bans
    if (roomToJoin.type === "GC") {
      // TODO check for bans
    }

    // check for blocks
    if (roomToJoin.type === "DM") {
      // check if user is blocked
      const blockered = await this.prisma.block.findFirst({
        where: { blockeeId: roomToJoin.ownerId, blockerId: user.id },
      });

      const blockeeed = await this.prisma.block.findFirst({
        where: { blockerId: roomToJoin.ownerId, blockeeId: user.id },
      });

      if (blockered || blockeeed)
        throw new BadRequestException("Unable to join room");
    }

    // check for previous join
    const prevJoin = await this.prisma.member.findFirst({
      where: { userId: user.id, roomId: roomToJoin.id },
    });
    if (prevJoin) throw new BadRequestException("User already in room");

    try {
      const res = await this.prisma.member.create({
        data: {
          userId: user.id,
          roomId: roomToJoin.id,
        },
      });

      return res;
    } catch (error) {
      throw new InternalServerErrorException(error.message);
    }
  }

  // remove member (leave room / kick)
  async removeMember(user: User, id: number) {
    // verify member exists
    const memberToDelete = await this.prisma.member.findUnique({
      include: { room: true },
      where: { id },
    });
    if (!memberToDelete) throw new NotFoundException("Member not found");
    if (memberToDelete.room.type === RoomType.DM)
      throw new BadRequestException("Cant leave Dms");

    // user is getting kicked
    if (memberToDelete.userId !== user.id) {
      // check for admin TODO
    }

    // user self leaving
    if (
      memberToDelete.userId === user.id &&
      memberToDelete.room.ownerId === user.id
    )
      throw new BadRequestException(
        "Ownership must be transfered before leaving"
      );

    const res = await this.prisma.member.delete({
      where: {
        id,
      },
    });

    return res;
  }
}
