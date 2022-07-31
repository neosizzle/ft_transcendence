import { Module } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { PrismaModule } from 'src/prisma/prisma.module';
import { PrismaService } from 'src/prisma/prisma.service';
import { RoomController } from './room.controller';

@Module({
  imports: [PrismaModule],
  controllers: [RoomController],
  providers: [PrismaService, ConfigService]
})
export class RoomModule {}
