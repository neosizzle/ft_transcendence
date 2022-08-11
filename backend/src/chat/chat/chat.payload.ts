import { Prisma } from "@prisma/client";
import { ListObject } from "src/utils";

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
  

export default generateChatPayload;