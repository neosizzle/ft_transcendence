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
import { AdminService } from "./admin.service";
import { AdminDto } from "./dto";

@UseGuards(AuthGuard)
@Controller("api/v1/admins")
export class AdminController {
  constructor(private adminService: AdminService) {}

  // list admins
  @Get()
  getAdmins(@Query() query: ListQuery) {
    return this.adminService.getAdmins(query);
  }

  // add admins (promote)
  @Post()
  createAdmin(@GetUser() user: User, @Body() dto: AdminDto) {
    return this.adminService.addAdmin(user, dto);
  }

  // delete admin (demote)
  @Delete(":id")
  deleteAdmin(@GetUser() user: User, @Param("id") id: string) {
    return this.adminService.removeAdmin(user, parseInt(id, 10));
  }
}
