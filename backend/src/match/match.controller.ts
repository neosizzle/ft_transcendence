import { Body, Controller, Get, Post, Query, UseGuards } from "@nestjs/common";
import { User } from "@prisma/client";
import { GetUser } from "src/users/auth/decorator";
import { AuthGuard } from "src/users/auth/guard";
import { ListQuery } from "src/utils";
import { MatchDto } from "./dto";
import { MatchService } from "./match.service";

@Controller("api/v1/matches")
@UseGuards(AuthGuard)
export class MatchController {
  constructor(private matchService: MatchService) {}

  // list matches
  @Get()
  getMatches(@Query() query: ListQuery) {
    return this.matchService.getMatches(query);
  }

  // add new match
  // @Post()
  // addMatch(@GetUser() user: User, @Body() dto: MatchDto) {
  //   return this.matchService.addMatch(user, dto);
  // }
}
