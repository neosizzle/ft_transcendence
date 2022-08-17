import { BadRequestException, Injectable } from "@nestjs/common";
import { Ban, Prisma, User } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListObject, ListQuery, validateListquery } from "src/utils";
import { MemberService } from "../member/member.service";
import { banDto } from "./ban.dto";

const transformFilterBan = (
  filterBy: string,
  filterOn: string
): string | number | boolean | Date => {
  if (filterOn === "expiresAt")
    return new Date(filterBy);
  if (filterOn === "id" || filterOn === "userId" || filterOn === "roomId")
    return parseInt(filterBy, 10);
  else return filterBy;
};

const generateBanWhere = (listObj: ListObject): Prisma.RoomWhereInput => {
  const res: Prisma.RoomWhereInput = {};

  if (listObj.operator === "AND") {
    res.AND = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterBan(
        listObj.filterBys[index],
        listObj.filterOns[index]
      );
      const filterOn = listObj.filterOns[index];
      const filterEntry = {};
      filterEntry[filterOn] = { equals: filterBy };
      res.AND.push(filterEntry);
    }
  } else if (listObj.operator === "OR") {
    res.OR = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterBan(
        listObj.filterBys[index],
        listObj.filterOns[index]
      );
      const filterOn = listObj.filterOns[index];
      const filterEntry = {};
      filterEntry[filterOn] = { equals: filterBy };
      res.OR.push(filterEntry);
    }
  }
  return res;
}

const generateBanPayload = (listObj: ListObject): Prisma.BanFindManyArgs => {
  const res: Prisma.BanFindManyArgs = {};

  res.skip = (listObj.page - 1) * listObj.pageSize;
  res.take = listObj.pageSize;
  // res.include = { user: true };
  if (listObj.filterBys) res.where = generateBanWhere(listObj);
  if (listObj.sortBy) {
    res.orderBy = [{}];
    res.orderBy[0][listObj.sortOn] =
      listObj.sortBy === "Ascending" ? "asc" : "desc";
  }
  return res;
};

@Injectable()
export class BanService {
  constructor(private readonly prismaService: PrismaService,
    private readonly memberService: MemberService) { }

  private sampleBan: Ban = {
    id: 0,
    userId: 0,
    roomId: 0,
    expiresAt: new Date(),
  };
  async getBan(query: ListQuery) {
    const keys = Object.keys(this.sampleBan).filter((e) => e);

    // validate query
    const listObj = validateListquery(query, keys);
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateBanPayload(listObj.data);
    const res = await this.prismaService.ban.findMany(payload);
    delete payload.skip;
    delete payload.take;
    delete payload.include;
    const total_elements = await this.prismaService.ban.count(
      <Prisma.BanCountArgs>payload
    );
    return { data: res, total_elements };
  }

  async giveBan(user: User, dto: banDto) {
    const userMember = await this.prismaService.member.findFirst({
      where: {
        userId: { equals: dto.userId },
        roomId: { equals: dto.roomId },
      }
    })
    if (!userMember)
      throw new BadRequestException("User is not in the room.");
    await this.memberService.removeMember(user, userMember.id);
    return await this.prismaService.ban.create({
      data: dto,
    });
  }
}
