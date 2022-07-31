import { Body, Controller, Get, Post } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { roomDto } from './room.dto';

@Controller('room')
export class RoomController {
    constructor(private readonly prismaService: PrismaService) {}

    @Get()
    getChat(): Promise<roomDto[]> {
        return this.prismaService.room.findMany();
    }
    @Post()
    create(@Body() {RoomName} : roomDto): Promise<roomDto> {
        return this.prismaService.room.create({
            data: {RoomName},
        });
    }
}
