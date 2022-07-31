import { Body, Controller, Get, Post } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { user_roomsDto } from './user_rooms.dto';

@Controller('user-rooms')
export class UserRoomsController {
    constructor(private readonly prismaService: PrismaService) {}

    @Get()
    getChat(): Promise<user_roomsDto[]> {
        return this.prismaService.user_rooms.findMany();
    }
    @Post()
    create(@Body() {RoomID, UserID} : user_roomsDto): Promise<user_roomsDto> {
        return this.prismaService.user_rooms.create({
            data: {RoomID, UserID},
        });
    }
}
