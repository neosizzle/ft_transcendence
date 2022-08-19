import {
  Body,
  Controller,
  Get,
  Param,
  Patch,
  Post,
  Query,
  UseGuards,
} from "@nestjs/common";
import { User } from "@prisma/client";
import { GetUser } from "src/users/auth/decorator";
import { AuthGuard } from "src/users/auth/guard";
import { ListQuery } from "src/utils";
import { roomPatchDto } from "./dto";
import { roomDto } from "./dto/room.dto";
import { RoomService } from "./room.service";

@Controller("api/v1/rooms")
@UseGuards(AuthGuard)
export class RoomController {
  constructor(private roomSercive: RoomService) {}

  // retreive rooms list based on filters, sorts, and pagination
  @Get()
  getRooms(@Query() query: ListQuery) {
    return this.roomSercive.getRooms(query);
  }

  // add a room
  @Post()
  addRoom(@GetUser() user: User, @Body() dto: roomDto) {
    return this.roomSercive.addRoom(user, dto);
  }

  // modify a room
  @Patch(":id")
  modifyRoom(
    @GetUser() user: User,
    @Param("id") id: string,
    @Body() dto: roomPatchDto
  ) {
    return this.roomSercive.modifyRoom(user, id, dto);
  }

  // do we need room deletion??
}
