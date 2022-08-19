import {
  BadRequestException,
  Injectable,
  NotFoundException,
} from "@nestjs/common";
import { FriendshipStatus, Prisma, User, UserStatus } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListObject, ListQuery, validateListquery } from "src/utils";
import { FriendsDto, FriendsPatchDto } from "./dto";

// TODO handle bad user status enum

const CURR_USER = 1;
const FRIEND_USER = 0;

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
  if (filterOn === "id" || filterOn === "level")
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
        throw new BadRequestException(
          "Bad status field. Must be 'LOGGEDIN', 'INGAME' or 'OFFLINE'"
        );
    }
  }
  if (filterOn === "createdAt" || filterOn === "updatedAt")
    return new Date(filterBy);
  else return filterBy;
};

const generateFilters = (
  listObj: ListObject,
  userType: number
): Prisma.FriendshipWhereInput => {
  const filterOptns: Prisma.FriendshipWhereInput = {};

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
      if (userType === CURR_USER) filterOptns.AND.push({ user: filterEntry });
      else filterOptns.AND.push({ friend: filterEntry });
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
      if (userType === CURR_USER) filterOptns.OR.push({ user: filterEntry });
      else filterOptns.OR.push({ friend: filterEntry });
    }
  }
  return filterOptns;
};

/**
 * Populates the 'where' option with filter values
 * @param listObj Listobject
 * @returns Generates the sorting field for the query payload
 */
const generateFriendsWhere = (
  listObj: ListObject,
  userId: number
): Prisma.FriendshipWhereInput => {
  // specify only to search for users containing me
  const res: Prisma.FriendshipWhereInput = {};
  res.AND = [];
  res.AND.push({
    OR: [
      { AND: [{ userId: userId }, generateFilters(listObj, FRIEND_USER)] },
      { AND: [{ friendId: userId }, generateFilters(listObj, CURR_USER)] },
    ],
  });
  res.AND.push({});
  res.AND[1].OR = [];

  // status filter
  for (let i = 0; listObj.filterOns && i < listObj.filterOns.length; i++) {
    const filterOn = listObj.filterOns[i];
    let filterBy: FriendshipStatus;

    if (filterOn == "reqStatus") {
      switch (listObj.filterBys[i]) {
        case "PENDING":
          filterBy = FriendshipStatus.PENDING;
          break;
        case "APPROVED":
          filterBy = FriendshipStatus.APPROVED;
          break;
        case "REJECTED":
          filterBy = FriendshipStatus.REJECTED;
          break;
        default:
          throw new BadRequestException(
            "Bad reqStatus field. Must be 'PENDING', 'APPROVED' or 'REJECTED'"
          );
      }
      res.AND[1].OR.push({ reqStatus: { equals: filterBy } });
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
const generateFriendPayload = (
  listObj: ListObject,
  userId: number
): Prisma.FriendshipFindManyArgs => {
  const res: Prisma.FriendshipFindManyArgs = {};

  res.include = {
    user: true,
    friend: true,
  };
  res.take = listObj.pageSize;
  res.skip = (listObj.page - 1) * listObj.pageSize;
  res.where = generateFriendsWhere(listObj, userId);
  if (listObj.sortBy) {
    res.orderBy = [{ friend: {} }];
    res.orderBy[0].friend[listObj.sortOn] =
      listObj.sortBy === "Ascending" ? "asc" : "desc";
  }
  return res;
};

@Injectable()
export class FriendsService {
  constructor(private prisma: PrismaService) {}

  // list friends
  async getFriends(user: User, query: ListQuery) {
    // validate query
    const listObj = validateListquery(query, [
      "reqStatus",
      ...Object.keys(user),
    ]);
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateFriendPayload(listObj.data, user.id);
    const res = await this.prisma.friendship.findMany(payload);
    delete payload.include;
    delete payload.take;
    delete payload.skip;
    const total_elements = await this.prisma.friendship.count(
      <Prisma.FriendshipCountArgs>payload
    );

    return { data: res, total_elements };
  }

  // add new friendship
  async addFriend(user: User, dto: FriendsDto) {
    // validate dto
    if (!dto.id && !dto.intraName)
      throw new BadRequestException("Need to provide intraName or id as body");

    if (dto.id && dto.intraName)
      throw new BadRequestException("only intra name or userid is needed");

    // check if user exist
    const payloadWhere = {};
    if (dto.id) payloadWhere["id"] = dto.id;
    else payloadWhere["intraName"] = dto.intraName;
    const userToAdd = await this.prisma.user.findFirst({
      where: payloadWhere,
    });

    if (!userToAdd) throw new NotFoundException("user not found");

    // check if user is self
    if (userToAdd.id == user.id)
      throw new BadRequestException(
        "Unable to send friend request to yourself"
      );

    // check if friendship existed
    const existingFriendship = await this.prisma.friendship.count({
      where: {
        OR: [
          {
            userId: { equals: user.id },
            friendId: { equals: userToAdd.id },
            OR: [
              { reqStatus: { equals: FriendshipStatus.APPROVED } },
              { reqStatus: { equals: FriendshipStatus.PENDING } },
            ],
          },
          {
            userId: { equals: userToAdd.id },
            friendId: { equals: user.id },
            OR: [
              { reqStatus: { equals: FriendshipStatus.APPROVED } },
              { reqStatus: { equals: FriendshipStatus.PENDING } },
            ],
          },
        ],
      },
    });
    if (existingFriendship)
      throw new BadRequestException("Already added friend");

    // create entry in friendship database
    const res = this.prisma.friendship.create({
      data: {
        userId: user.id,
        friendId: userToAdd.id,
      },
    });

    return res;
  }

  // delete friendship (unfriend)
  async removeFriend(user: User, friendId: number) {
    // check if user trying to remove self
    if (user.id === friendId)
      throw new BadRequestException("Cannot remove self");

    // delete all friend relations where between userid and friendid
    const res = await this.prisma.friendship.deleteMany({
      where: {
        OR: [
          {
            userId: user.id,
            friendId: friendId,
          },
          {
            userId: friendId,
            friendId: user.id,
          },
        ],
      },
    });

    return res;
  }

  // modify friendship (should be only used for accepting / rejecting requests)
  async modifyFriend(user: User, id: number, dto: FriendsPatchDto) {
    //check if user is modifying self
    if (user.id === dto.friendId)
      throw new BadRequestException("Cannot modify self");

    //check if user has permission to modify friendship
    const friendship = await this.prisma.friendship.findFirst({
      where: {
        id: id,
        friendId: user.id,
        userId: dto.friendId,
      },
    });
    if (!friendship) throw new NotFoundException();

    // validate correct req status
    if (!Object.values(FriendshipStatus).find((e) => e === dto.reqStatus))
      throw new BadRequestException("Bad request status");

    const res = await this.prisma.friendship.update({
      where: {
        id: id,
      },
      include: {
        user: true,
        friend: true,
      },
      data: {
        reqStatus: <FriendshipStatus>dto.reqStatus,
      },
    });
    return res;
  }
}
