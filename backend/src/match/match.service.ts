import {
  BadRequestException,
  Injectable,
  InternalServerErrorException,
  NotFoundException,
} from "@nestjs/common";
import { Match, Prisma, User } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListObject, ListQuery, validateListquery } from "src/utils";
import { MatchDto } from "./dto";

const RANK_CONST_GAIN = 42;

/**
 * Transforms string values into values of their specific type
 *
 * @param filterBy Filterby string
 * @param filterOn Filteron string
 * @returns
 */
const transformFilterMatch = (
  filterBy: string,
  filterOn: string
): string | number | boolean | Date => {
  if (filterOn === "isCustom") return filterBy === "true";
  if (filterOn === "createdAt" || filterOn === "updatedAt")
    return new Date(filterBy);
  else return parseInt(filterBy, 10);
};

/**
 * Populates the 'where' option with filter values
 * @param listObj Listobject
 * @returns Generates the sorting field for the query payload
 */
const generateMatchWhere = (listObj: ListObject): Prisma.MatchWhereInput => {
  const res: Prisma.MatchWhereInput = {};

  if (listObj.operator === "AND") {
    res.AND = [];
    for (
      let index = 0;
      listObj.filterBys && index < listObj.filterBys.length;
      index++
    ) {
      const filterBy = transformFilterMatch(
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
      const filterBy = transformFilterMatch(
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
};

/**
 * Generates pagination settings
 * Generates filters
 * Generates sorts
 *
 * @param listObj List object
 * @returns Generated find query payload
 */
const generateMatchPayload = (
  listObj: ListObject
): Prisma.MatchFindManyArgs => {
  const res: Prisma.MatchFindManyArgs = {};

  res.take = listObj.pageSize;
  res.skip = (listObj.page - 1) * listObj.pageSize;
  res.include = { player0: true, player1: true, winner: true };
  if (listObj.filterBys) res.where = generateMatchWhere(listObj);
  if (listObj.sortBy) {
    res.orderBy = [{}];
    res.orderBy[0][listObj.sortOn] =
      listObj.sortBy === "Ascending" ? "asc" : "desc";
  }
  return res;
};

@Injectable()
export class MatchService {
  constructor(private prisma: PrismaService) {}

  private sampleMatch: Match = {
    id: 0,
    playerId0: 0,
    playerId1: 0,
    playerScore0: 0,
    playerScore1: 0,
    winnerId: 0,
    isCustom: false,
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  // get matches
  async getMatches(query: ListQuery) {
    const keys = Object.keys(this.sampleMatch);

    // validate query
    const listObj = validateListquery(query, keys);
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateMatchPayload(listObj.data);
    const res = await this.prisma.match.findMany(payload);
    delete payload.skip;
    delete payload.take;
    delete payload.include;
    const total_elements = await this.prisma.match.count(
      <Prisma.MatchCountArgs>payload
    );
    return { data: res, total_elements };
  }

  // add match entry
  async addMatch(dto: MatchDto) {

    // check if both user ids are not the same
    if (dto.playerId0 === dto.playerId1)
      throw new BadRequestException("Player0 and player1 must be different");

    // check if both player exists
    const player0 = await this.prisma.user.findUnique({where : { id : dto.playerId0 }});
    const player1 = await this.prisma.user.findUnique({where : { id : dto.playerId1 }});

    if (!player0 || !player1) throw new NotFoundException("user not found");

    // check if winner is either of player ids
    if (dto.winnerId !== dto.playerId0 && dto.winnerId !== dto.playerId1)
      throw new BadRequestException("Invalid winner");

    // set winner user 
    let winner : User;
    let loser : User;
    if (dto.winnerId === player0.id)
    {
      winner = player0;
      loser = player1;
    }
    else{
      winner = player1;
      loser = player0;
    }

    // Formula for new ranking calculation: (rank of losing player / rank of winning player) * 42 (constant) * ((score difference/100) + 1) Cred. Wallyboy
    // update winner ranking and winloss and level
    const rankGain = (loser.ranking / winner.ranking ? loser.ranking / winner.ranking : 1) * RANK_CONST_GAIN * (Math.abs(dto.playerScore0 - dto.playerScore1) + 1);

    try {
      await this.prisma.user.update({
        where : {
          id : winner.id
        },
        data : {
          ranking : winner.ranking + rankGain,
          wins : winner.wins + 1,
          level : winner.level + 0.2,
        }
      })

      await this.prisma.user.update({
        where : {
          id : loser.id
        },
        data : {
          ranking : loser.ranking - rankGain,
          losses : loser.losses + 1,
          level : loser.level + 0.2,
        }
      })
    } catch (error) {
      console.error(error);
      throw new InternalServerErrorException(error.message);
    }

    // add to db
    try {
      const res = await this.prisma.match.create({
        data: dto,
      });      
      return res;
    } catch (error) {
      console.error(error);
      throw new InternalServerErrorException(error.message);
    }
  }
}
