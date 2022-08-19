import { Module } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PrismaService } from "src/prisma/prisma.service";
import { RoomController } from "./room.controller";
import { RoomService } from "./room.service";

@Module({
  controllers: [RoomController],
  providers: [RoomService, PrismaService, ConfigService],
})
export class RoomModule {}
