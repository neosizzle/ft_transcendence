import { Module } from '@nestjs/common';
import { ConfigService } from "@nestjs/config";
import { GameEventsGateway } from './events.gateway';
import { PrismaService } from "src/prisma/prisma.service";

@Module({
  providers: [
    PrismaService,
    ConfigService,
    GameEventsGateway,
  ],
})
export class GameEventsModule {}
