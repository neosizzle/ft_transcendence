import { Module } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { BlocksService } from 'src/blocks/blocks.service';
import { PrismaModule } from 'src/prisma/prisma.module';
import { PrismaService } from 'src/prisma/prisma.service';
import { ChatController } from './chat.controller';
import { ChatGateway } from './chat.gateway';
@Module({
  imports: [PrismaModule],
  controllers: [ChatController],
  providers: [PrismaService, ConfigService, ChatGateway, BlocksService]
})
export class ChatModule {}
