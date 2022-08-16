import { Module } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PrismaService } from "src/prisma/prisma.service";
import { AdminService } from "../admin/admin.service";
import { MemberController } from "./member.controller";
import { MemberService } from "./member.service";

@Module({
  controllers: [MemberController],
  providers: [MemberService, PrismaService, ConfigService, AdminService],
})
export class MemberModule {}
