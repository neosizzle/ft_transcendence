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
import { AdminService } from "../admin/admin.service";

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
  res.include = {
    user: true,
    room: {
      select: {
        id: true,
        roomName: true,
        ownerId: true,
        type: true,
        isProtected: true,
        createdAt: true,
        updatedAt: true,
        owner: true,
        password: false,
      },
    },
  };
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
  constructor(private prisma: PrismaService, private admin: AdminService) {}

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
      const banRes = await this.prisma.ban.findFirst({
        where: {
          userId: user.id,
          roomId: roomToJoin.id,
          expiresAt: { gt: new Date() },
        },
      });

      if (banRes)
        throw new BadRequestException(
          "You are banned from this room until " + new Date(banRes.expiresAt).toLocaleString("en-gb", { hour12: true, timeZone: "Asia/Singapore"}) + "."
          //Can't convert it to localtimezone for some reason?
        );
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
        include : {
          user : true,
          room : true
        }
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
      // find that user.id is an admin on current room
      const isUserAdmin = await this.admin.getAdmins({
        page: "1",
        pageSize: "1",
        filterOn: "userId,roomId",
        filterBy: `${user.id},${memberToDelete.roomId}`,
      });

      if (!isUserAdmin.total_elements)
        throw new UnauthorizedException("Not eligible to perform action");

      // find that memberToDelete is not room owner
      if (memberToDelete.room.ownerId === memberToDelete.userId)
        throw new BadRequestException("Cannot remove owner");
    }

    // user self leaving
    const roomMembersCnt = await this.prisma.member.count({
      where: { roomId: memberToDelete.roomId },
    });

    if (
      memberToDelete.userId === user.id &&
      memberToDelete.room.ownerId === user.id &&
      roomMembersCnt > 1
    ) {
      // if user is owner of room, deny leave if we have more than one member
      throw new BadRequestException(
        "Ownership must be transfered before leaving"
      );
    }

    // if user is admin in current room, delete the admin as well
    await this.prisma.admin.deleteMany({
      where: {
        userId: memberToDelete.userId,
        roomId: memberToDelete.roomId,
      },
    });

    const res = await this.prisma.member.delete({
      where: {
        id,
      },
    });

    // if we only have 1 room member, delete room
    if (
      memberToDelete.userId === user.id &&
      memberToDelete.room.ownerId === user.id &&
      roomMembersCnt === 1
    )
      await this.prisma.room.delete({ where: { id: memberToDelete.roomId } });
    return res;
  }
}
