import {
  Body,
  Controller,
  Delete,
  Get,
  Param,
  Post,
  Query,
  UseGuards,
} from "@nestjs/common";
import { User } from "@prisma/client";
import { GetUser } from "src/users/auth/decorator";
import { AuthGuard } from "src/users/auth/guard";
import { ListQuery } from "src/utils";
import { BlocksService } from "./blocks.service";
import { BlocksDto } from "./dto";

@Controller("/api/v1/blocks")
@UseGuards(AuthGuard)
export class BlocksController {
  constructor(private blockService: BlocksService) {}

  // lists out blocks of current user
  @Get()
  getBlocks(@GetUser() user: User, @Query() query: ListQuery) {
    return this.blockService.getBlocks(user, query);
  }

  // adds new block
  @Post()
  addBlock(@GetUser() user: User, @Body() dto: BlocksDto) {
    return this.blockService.addBlock(user, dto);
  }

  @Delete(":id")
  deleteBlock(@GetUser() user: User, @Param("id") id: string) {
    return this.blockService.removeBlock(user, parseInt(id, 10));
  }
}
