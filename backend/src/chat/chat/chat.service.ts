import { BadRequestException, Injectable } from "@nestjs/common";
import { Chat, Prisma } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListObject, ListQuery, validateListquery } from "src/utils";
import { chatDto } from "./chat.dto";

const transformFilterChat = (
  filterBy: string,
  filterOn: string
): string | number | boolean | Date => {
  if (filterOn === "createdAt" || filterOn === "updatedAt")
    return new Date(filterBy);
  if (filterOn === "id" || filterOn === "userId" || filterOn === "roomId")
    return parseInt(filterBy, 10);
  else return filterBy;
};

const generateChatWhere = (listObj: ListObject): Prisma.RoomWhereInput => {
  const res: Prisma.RoomWhereInput = {};

  if (listObj.operator === "AND") {
    res.AND = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterChat(
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
      const filterBy = transformFilterChat(
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
}

const generateChatPayload = (listObj: ListObject): Prisma.ChatFindManyArgs => {
  const res: Prisma.ChatFindManyArgs = {};

  res.skip = (listObj.page - 1) * listObj.pageSize;
  res.take = listObj.pageSize;
  // res.include = { user: true };
  if (listObj.filterBys) res.where = generateChatWhere(listObj);
  if (listObj.sortBy) {
    res.orderBy = [{}];
    res.orderBy[0][listObj.sortOn] =
      listObj.sortBy === "Ascending" ? "asc" : "desc";
  }
  return res;
};

@Injectable()
export class ChatService {
  constructor(private readonly prismaService: PrismaService) {}

  private sampleChat: Chat = {
    id: 0,
    userId: 0,
    roomId: 0,
    message: "",
    createdAt: new Date(),
    updatedAt: new Date(),
  };
  async getChat(query: ListQuery) {
    const keys = Object.keys(this.sampleChat).filter((e) => e);

    // validate query
    const listObj = validateListquery(query, keys);
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateChatPayload(listObj.data);
    const res = await this.prismaService.chat.findMany(payload);
    delete payload.skip;
    delete payload.take;
    delete payload.include;
    const total_elements = await this.prismaService.chat.count(
      <Prisma.ChatCountArgs>payload
    );
    return { data: res, total_elements };
  }
  insertChat(dto: chatDto): Promise<chatDto> {
    return this.prismaService.chat.create({
      data: dto,
    });
  }
}
