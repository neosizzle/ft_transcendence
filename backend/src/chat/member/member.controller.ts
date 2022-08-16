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
import { MemberDto } from "./dto";
import { MemberService } from "./member.service";

@Controller("api/v1/members")
@UseGuards(AuthGuard)
export class MemberController {
  constructor(private membersService: MemberService) {}

  // list members for a user / for a room
  @Get()
  getMembers(@GetUser() user: User, @Query() query: ListQuery) {
    return this.membersService.getMembers(query);
  }

  // add member (user joins room)
  @Post()
  addMember(@GetUser() user: User, @Body() dto: MemberDto) {
    return this.membersService.addMember(user, dto);
  }

  // remove member (user leaves room)
  @Delete(":id")
  removeMember(@GetUser() user: User, @Param("id") id: string) {
    return this.membersService.removeMember(user, parseInt(id, 10));
  }
}
