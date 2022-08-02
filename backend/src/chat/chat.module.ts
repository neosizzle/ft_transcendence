import { Module } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { BlocksService } from 'src/users/blocks/blocks.service';
import { PrismaModule } from 'src/prisma/prisma.module';
import { PrismaService } from 'src/prisma/prisma.service';
import { ChatController } from './chat.controller';
import { ChatGateway } from './chat.gateway';
import { ChatService } from './chat.service';
@Module({
  imports: [PrismaModule],
  controllers: [ChatController],
  providers: [PrismaService, ConfigService, ChatGateway, BlocksService, ChatService]
})
export class ChatModule {}
