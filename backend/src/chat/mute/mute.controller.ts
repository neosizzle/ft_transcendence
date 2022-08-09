import { Body, Controller, Get, Post, Query } from "@nestjs/common";
import { PrismaService } from "src/prisma/prisma.service";
import { ListQuery } from "src/utils";
import { muteDto } from "./mute.dto";
import { MuteService } from "./mute.service";

@Controller("mute")
export class MuteController {
  constructor(
    private readonly prismaService: PrismaService,
    private readonly muteService: MuteService
  ) {}

  @Get()
  getMute(@Query() query: ListQuery) {
    return this.muteService.getMute(query);
  }
  @Post()
  create(@Body() muteDto: muteDto): Promise<muteDto> {
    return this.muteService.giveMute(muteDto);
  }
}
