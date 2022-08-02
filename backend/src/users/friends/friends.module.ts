import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { PrismaService } from 'src/prisma/prisma.service';
import { FriendsController } from './friends.controller';
import { FriendsService } from './friends.service';

@Module({
  imports: [ConfigModule],
  controllers: [FriendsController],
  providers: [FriendsService, PrismaService]
})
export class FriendsModule {}
