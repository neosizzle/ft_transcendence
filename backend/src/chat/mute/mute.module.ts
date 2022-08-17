import { Module } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PrismaModule } from "src/prisma/prisma.module";
import { PrismaService } from "src/prisma/prisma.service";
import { AdminService } from "../admin/admin.service";
import { MuteController } from "./mute.controller";
import { MuteService } from "./mute.service";

@Module({
  imports: [PrismaModule],
  controllers: [MuteController],
  providers: [PrismaService, ConfigService, MuteService, AdminService],
})
export class MuteModule {}
