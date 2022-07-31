import { Module } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { PrismaModule } from 'src/prisma/prisma.module';
import { PrismaService } from 'src/prisma/prisma.service';
import { UserRoomsController } from './user_rooms.controller';

@Module({
  imports: [PrismaModule],
  controllers: [UserRoomsController],
  providers: [PrismaService, ConfigService]
})
export class UserRoomsModule {}
