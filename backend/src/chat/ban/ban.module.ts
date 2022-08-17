import { Module } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PrismaModule } from "src/prisma/prisma.module";
import { PrismaService } from "src/prisma/prisma.service";
import { MemberService } from "../member/member.service";
import { BanController } from "./ban.controller";
import { BanService } from "./ban.service";

@Module({
  imports: [PrismaModule],
  controllers: [BanController],
  providers: [PrismaService, ConfigService, BanService, MemberService],
})
export class BanModule {}
