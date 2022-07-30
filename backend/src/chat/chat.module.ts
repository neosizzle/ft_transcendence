import { Module } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { PrismaModule } from 'src/prisma/prisma.module';
import { PrismaService } from 'src/prisma/prisma.service';
import { ChatController } from './chat.controller';
import { ChatGateway } from './chat.gateway';

@Module({
  imports: [PrismaModule],
  controllers: [ChatController],
  providers: [PrismaService, ConfigService, ChatGateway]
})
export class ChatModule {}
