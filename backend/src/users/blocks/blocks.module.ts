import { Module } from "@nestjs/common";
import { ConfigModule } from "@nestjs/config";
import { PrismaService } from "src/prisma/prisma.service";
import { BlocksController } from "./blocks.controller";
import { BlocksService } from "./blocks.service";

@Module({
  imports: [ConfigModule],
  controllers: [BlocksController],
  providers: [BlocksService, PrismaService],
})
export class BlocksModule {}
