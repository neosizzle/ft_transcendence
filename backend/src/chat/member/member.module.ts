import { Module } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PrismaService } from "src/prisma/prisma.service";
import { MemberController } from "./member.controller";
import { MemberService } from "./member.service";

@Module({
  controllers: [MemberController],
  providers: [MemberService, PrismaService, ConfigService],
})
export class MemberModule {}
