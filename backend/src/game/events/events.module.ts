import { Module } from '@nestjs/common';
import { ConfigService } from "@nestjs/config";
import { GameEventsGateway } from './events.gateway';
import { PrismaService } from "src/prisma/prisma.service";
import { UsersService } from 'src/users/users/users.service';

@Module({
  providers: [
    UsersService,
    PrismaService,
    ConfigService,
    GameEventsGateway,
  ],
})
export class GameEventsModule {}
