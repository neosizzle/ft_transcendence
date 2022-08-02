import { Body, Controller, Get, Post } from '@nestjs/common';
import { Chat } from '@prisma/client';
import { PrismaService } from 'src/prisma/prisma.service';
import { chatDto } from './chat.dto';
import { ChatService } from './chat.service';

@Controller('chat')
export class ChatController {
    constructor(private readonly prismaService: PrismaService, private readonly chatService: ChatService) {}

    @Get()
    getChat(): Promise<Chat[]>{
        return this.prismaService.chat.findMany()
    }
    @Post()
    create(@Body() chatDto : chatDto): Promise<chatDto> {   
        return (this.chatService.insertChat(chatDto));
    }
}