import {
  BadRequestException,
  Injectable,
  InternalServerErrorException,
  NotFoundException,
  UnauthorizedException,
} from "@nestjs/common";
import { Admin, Prisma, User } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListObject, ListQuery, validateListquery } from "src/utils";
import { AdminDto } from "./dto";

/**
 * Transforms string values into values of their specific type
 *
 * @param filterBy Filterby string
 * @param filterOn Filteron string
 * @returns
 */
const transformFilterAdmin = (
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
const generateAdminWhere = (listObj: ListObject): Prisma.AdminWhereInput => {
  const res: Prisma.AdminWhereInput = {};

  if (listObj.operator === "AND") {
    res.AND = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterAdmin(
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
      const filterBy = transformFilterAdmin(
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
const generateAdminPayload = (
  listObj: ListObject
): Prisma.AdminFindManyArgs => {
  const res: Prisma.AdminFindManyArgs = {};

  res.take = listObj.pageSize;
  res.skip = (listObj.page - 1) * listObj.pageSize;
  res.include = { user: true, room: true };
  if (listObj.filterBys) res.where = generateAdminWhere(listObj);
  if (listObj.sortBy) {
    res.orderBy = [{}];
    res.orderBy[0][listObj.sortOn] =
      listObj.sortBy === "Ascending" ? "asc" : "desc";
  }
  return res;
};

@Injectable()
export class AdminService {
  constructor(private prisma: PrismaService) {}

  private sampleAdmin: Admin = {
    id: -1,
    userId: -1,
    roomId: -1,
  };

  // list admin
  async getAdmins(query: ListQuery) {
    const keys = Object.keys(this.sampleAdmin);

    // validate query
    const listObj = validateListquery(query, keys);
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateAdminPayload(listObj.data);
    const res = await this.prisma.admin.findMany(payload);
    delete payload.skip;
    delete payload.take;
    delete payload.include;
    const total_elements = await this.prisma.admin.count(
      <Prisma.AdminCountArgs>payload
    );
    return { data: res, total_elements };
  }

  // add admin (promote)
  async addAdmin(user: User, dto: AdminDto) {
    // check if room exist
    const room = await this.prisma.room.findFirst({
      where: { id: dto.roomId },
    });
    if (!room) throw new NotFoundException("Room not found");

    // deny if room is dm
    if (room.type !== "GC") throw new BadRequestException("DM Is not allowed");

    // check if user is owner of said room
    if (room.ownerId !== user.id)
      throw new UnauthorizedException("Permission denied");

    // check if user to promote exists in member and is not admin
    const memberToPromote = await this.prisma.member.findFirst({
      where: { userId: dto.userId, roomId: room.id },
    });
    if (!memberToPromote) throw new NotFoundException("User not found");

    const isUserAdmin = await this.prisma.admin.findFirst({
      where: { userId: memberToPromote.userId, roomId: room.id },
    });
    if (isUserAdmin) throw new BadRequestException("User is already admin");

    // add in db
    try {
      const res = await this.prisma.admin.create({
        data: dto,
      });
      return res;
    } catch (error) {
      console.error(error);
      throw new InternalServerErrorException(error.message);
    }
  }

  // remove admin (demote)
  async removeAdmin(user: User, id: number) {
    // verify that admin exists and get room
    const admin = await this.prisma.admin.findUnique({ where: { id } });
    if (!admin) throw new NotFoundException("Admin not found");

    // verify that room owner is user
    const room = await this.prisma.room.findUnique({
      where: { id: admin.roomId },
    });
    if (!room) throw new NotFoundException("Room not found");
    if (room.ownerId !== user.id)
      throw new UnauthorizedException("Permission denied");

    // cannot demote self
    if (admin.userId === user.id)
      throw new BadRequestException("Cannt demote self");

    // delete admin
    try {
      const res = await this.prisma.admin.delete({ where: { id } });
      return res;
    } catch (error) {
      console.error(error);
      throw new InternalServerErrorException(error.message);
    }
  }
}
