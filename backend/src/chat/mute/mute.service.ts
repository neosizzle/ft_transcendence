import { BadRequestException, Injectable } from "@nestjs/common";
import { Prisma, Mute } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListObject, ListQuery, validateListquery } from "src/utils";
import { muteDto } from "./mute.dto";

const transformFilterMute = (
  filterBy: string,
  filterOn: string
): string | number | boolean | Date => {
  if (filterOn === "expiresAt")
    return new Date(filterBy);
  if (filterOn === "id" || filterOn === "userId" || filterOn === "roomId")
    return parseInt(filterBy, 10);
  else return filterBy;
};

const generateMuteWhere = (listObj: ListObject): Prisma.RoomWhereInput => {
  const res: Prisma.RoomWhereInput = {};

  if (listObj.operator === "AND") {
    res.AND = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterMute(
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
      const filterBy = transformFilterMute(
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

const generateMutePayload = (listObj: ListObject): Prisma.MuteFindManyArgs => {
  const res: Prisma.MuteFindManyArgs = {};

  res.skip = (listObj.page - 1) * listObj.pageSize;
  res.take = listObj.pageSize;
  // res.include = { user: true };
  if (listObj.filterBys) res.where = generateMuteWhere(listObj);
  if (listObj.sortBy) {
    res.orderBy = [{}];
    res.orderBy[0][listObj.sortOn] =
      listObj.sortBy === "Ascending" ? "asc" : "desc";
  }
  return res;
};

@Injectable()
export class MuteService {
  constructor(private readonly prismaService: PrismaService) {}

  private sampleMute: Mute = {
    id: 0,
    userId: 0,
    roomId: 0,
    expiresAt: new Date(),
  };
  async getMute(query: ListQuery) {
    const keys = Object.keys(this.sampleMute).filter((e) => e);

    // validate query
    const listObj = validateListquery(query, keys);
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateMutePayload(listObj.data);
    const res = await this.prismaService.mute.findMany(payload);
    delete payload.skip;
    delete payload.take;
    delete payload.include;
    const total_elements = await this.prismaService.mute.count(
      <Prisma.MuteCountArgs>payload
    );
    return { data: res, total_elements };
  }
  giveMute(dto: muteDto): Promise<muteDto> {
    //Note that "2022-08-02 doesn't work. Must be complete ISO8601 string, 2022-08-02T05:42:38.573Z"
    return this.prismaService.mute.create({
      data: dto,
    });
  }
}
