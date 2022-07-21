import { Body, Controller, Get, Post } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { chatDto } from './chat.dto';

@Controller('chat')
export class ChatController {
    constructor(private readonly prismaService: PrismaService) {}

    @Get()
    getChat(): Promise<chatDto[]> {
        return this.prismaService.chat.findMany();
    }
    @Post()
    create(@Body() {UserID, timestamp, message} : chatDto): Promise<chatDto> {
        return this.prismaService.chat.create({
            data: {UserID, timestamp, message},
        });
    }
}
