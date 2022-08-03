import {
  BadRequestException,
  Injectable,
  InternalServerErrorException,
  NotFoundException,
  UnauthorizedException,
} from "@nestjs/common";
import { Prisma, Room, RoomType, User } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListObject, ListQuery, validateListquery } from "src/utils";
import { roomDto } from "./dto/room.dto";
import * as CryptoJS from "crypto-js";
import { roomPatchDto } from "./dto";

/**
 * Transforms string values into values of their specific type
 *
 * @param filterBy Filterby string
 * @param filterOn Filteron string
 * @returns
 */
const transformFilterRoom = (
  filterBy: string,
  filterOn: string
): string | number | boolean | Date => {
  if (filterOn === "isProtected") return filterBy === "true";
  if (filterOn === "type") {
    switch (filterBy) {
      case "DM":
        return RoomType.DM;
      case "GC":
        return RoomType.GC;
      default:
        throw new BadRequestException("Bad status field. Must be 'DM' or 'GC'");
    }
  }
  if (filterOn === "createdAt" || filterOn === "updatedAt")
    return new Date(filterBy);
  if (filterOn === "id" || filterOn === "ownerId")
    return parseInt(filterBy, 10);
  else return filterBy;
};

/**
 * Populates the 'where' option with filter values
 * @param listObj Listobject
 * @returns Generates the sorting field for the query payload
 */
const generateRoomWhere = (listObj: ListObject): Prisma.RoomWhereInput => {
  const res: Prisma.RoomWhereInput = {};

  if (listObj.operator === "AND") {
    res.AND = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterRoom(
        listObj.filterBys[index],
        listObj.filterOns[index]
      );
      const filterOn = listObj.filterOns[index];
      const filterEntry = {};
      if (filterBy == "GC" || filterBy == "DM" || typeof filterBy !== "string")
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
      const filterBy = transformFilterRoom(
        listObj.filterBys[index],
        listObj.filterOns[index]
      );
      const filterOn = listObj.filterOns[index];
      const filterEntry = {};
      if (filterBy == "GC" || filterBy == "DM" || typeof filterBy !== "string")
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
const generateRoomPayload = (listObj: ListObject): Prisma.RoomFindManyArgs => {
  const res: Prisma.RoomFindManyArgs = {};

  res.take = listObj.pageSize;
  res.skip = (listObj.page - 1) * listObj.pageSize;
  res.include = { owner: true };
  if (listObj.filterBys) res.where = generateRoomWhere(listObj);
  if (listObj.sortBy) {
    res.orderBy = [{}];
    res.orderBy[0][listObj.sortOn] =
      listObj.sortBy === "Ascending" ? "asc" : "desc";
  }
  return res;
};

@Injectable()
export class RoomService {
  constructor(private prisma: PrismaService) {}

  private sampleRoom: Room = {
    id: 0,
    roomName: "",
    password: "",
    ownerId: 0,
    type: "DM",
    isProtected: false,
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  // get rooms with filter, pagination and sort
  async getRooms(query: ListQuery) {
    const keys = Object.keys(this.sampleRoom).filter((e) => e !== "password");

    // validate query
    const listObj = validateListquery(query, keys);
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateRoomPayload(listObj.data);
    console.log(payload.where.OR);
    const res = await this.prisma.room.findMany(payload);
    delete payload.skip;
    delete payload.take;
    const total_elements = await this.prisma.room.count(
      <Prisma.RoomCountArgs>payload
    );

    return { data: res, total_elements };
  }

  // add room to database
  async addRoom(user: User, dto: roomDto) {
    // if we have a password, hash it
    if (dto.password) dto.password = CryptoJS.SHA256(dto.password).toString();

    // check if trying to create roomname with dm
    if (dto.roomName && dto.type === RoomType.DM)
      throw new BadRequestException("No roomnames for DMs");
    else if (!dto.roomName && dto.type === RoomType.GC)
      throw new BadRequestException("Roomname needed for gcs");

    // obtain initial users
    let initUsers: string[];
    if (dto.initialUsers) initUsers = dto.initialUsers.split(",");

    // check if existing user in DM
    if (dto.type === RoomType.DM && !initUsers)
      throw new BadRequestException("DM Must be created with an initialuser");
    if (dto.type === RoomType.DM) {
      const commonDMRooms = await this.prisma.member.findMany({
        where: {
          AND: [
            {
              OR: [
                {
                  userId: user.id,
                },
                {
                  userId: parseInt(initUsers[0]),
                },
              ],
            },
            {
              room: {
                type: RoomType.DM,
              },
            },
          ],
        },
      });

      if (commonDMRooms.length > 1)
        throw new BadRequestException("DM already exist");

      // dm should have no password
      if (dto.password)
        throw new BadRequestException("Password forbidden for DMs");

      // check if user is blocked
      const blockered = await this.prisma.block.findFirst({
        where: { blockeeId: parseInt(initUsers[0]), blockerId: user.id },
      });

      const blockeeed = await this.prisma.block.findFirst({
        where: { blockerId: parseInt(initUsers[0]), blockeeId: user.id },
      });

      if (blockered || blockeeed)
        throw new BadRequestException("Unable to create room");
    }
    //  TODO if GC, make current user admin of room

    try {
      // create room in db
      const res = await this.prisma.room.create({
        data: {
          roomName: dto.roomName,
          password: dto.password,
          ownerId: user.id,
          type: dto.type as RoomType,
          isProtected: dto.password ? true : false,
        },
      });

      //add initial users
      if (dto.initialUsers) {
        // check if attempt to have multiple intial users in DM
        if (initUsers.length > 1 && dto.type == RoomType.DM)
          throw new BadRequestException("Cannot add initial users in DM");

        const payload: Prisma.MemberCreateManyInput[] = [];
        payload.push({ userId: user.id, roomId: res.id });
        initUsers.forEach((e) =>
          payload.push({
            userId: parseInt(e),
            roomId: res.id,
          } as Prisma.MemberCreateManyInput)
        );

        await this.prisma.member.createMany({
          data: payload,
        });
      }
      return res;
    } catch (error) {
      console.error(error);
      if (error.code === "P2002")
        throw new BadRequestException("Roomname already exists");
      else if (error.code === "P2003")
        throw new BadRequestException("Invalid user in initialUsers");
      else if (error instanceof BadRequestException) throw error;
      throw new InternalServerErrorException(error.message);
    }
  }

  // modify room status
  async modifyRoom(user: User, id: string, dto: roomPatchDto) {
    // if we have a password, hash it
    if (dto.password) dto.password = CryptoJS.SHA256(dto.password).toString();

    // get original room
    const originalRoom = await this.prisma.room.findUnique({
      where: {
        id: parseInt(id),
      },
    });

    // no room
    if (!originalRoom) throw new NotFoundException();

    // check for owner status
    if (originalRoom.ownerId !== user.id)
      throw new UnauthorizedException("Operation not allowed");

    // validate roomname change
    if (originalRoom.type === "DM")
      throw new BadRequestException("Cant change a DM's properties");

    // validate password
    if (dto.isProtected != undefined && !dto.isProtected) dto.password = null;
    if (dto.isProtected && !dto.password)
      throw new BadRequestException("Please provide a password");

    // update changes in db
    try {
      const res = await this.prisma.room.update({
        data: dto,
        where: {
          id: parseInt(id),
        },
      });
      return res;
    } catch (error) {
      console.error(error);
      if (error.code === "P2003")
        throw new BadRequestException("Invalid user in ownerId");
      throw new InternalServerErrorException(error.message);
    }
  }
}
