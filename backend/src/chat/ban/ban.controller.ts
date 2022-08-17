import { Body, Controller, Get, Post, Query, UseGuards } from "@nestjs/common";
import { User } from "@prisma/client";
import { GetUser } from "src/users/auth/decorator";
import { AuthGuard } from "src/users/auth/guard";
import { ListQuery } from "src/utils";
import { banDto } from "./ban.dto";
import { BanService } from "./ban.service";

@Controller("api/v1/ban")
@UseGuards(AuthGuard)
export class BanController {
  constructor(
    private readonly banService: BanService
  ) {}

  @Get()
  getBan(@Query() query: ListQuery) {
    return this.banService.getBan(query);
  }
  @Post()
  create(@GetUser() user : User, @Body() banDto: banDto): Promise<banDto> {
    return this.banService.giveBan(user, banDto);
  }
}
