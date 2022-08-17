import { Body, Controller, Get, Post, Query, UseGuards } from "@nestjs/common";
import { User } from "@prisma/client";
import { GetUser } from "src/users/auth/decorator";
import { AuthGuard } from "src/users/auth/guard";
import { ListQuery } from "src/utils";
import { muteDto } from "./mute.dto";
import { MuteService } from "./mute.service";

@Controller("api/v1/mute")
@UseGuards(AuthGuard)
export class MuteController {
  constructor(
    private readonly muteService: MuteService
  ) {}

  @Get()
  getMute(@Query() query: ListQuery) {
    return this.muteService.getMute(query);
  }
  @Post()
  create(@GetUser() user: User, @Body() muteDto: muteDto): Promise<muteDto> {
    return this.muteService.giveMute(user, muteDto);
  }
}
