import { Body, Controller, Get, Post } from '@nestjs/common';
import { Mute } from '@prisma/client';
import { PrismaService } from 'src/prisma/prisma.service';
import { muteDto } from './mute.dto';
import { MuteService } from './mute.service';

@Controller('mute')
export class MuteController {
    constructor(
        private readonly prismaService: PrismaService,
        private readonly muteService: MuteService
    ) { }

    @Get()
    getMute(): Promise<Mute[]> {
        return this.prismaService.mute.findMany();
    }
    @Post()
    create(@Body() muteDto: muteDto): Promise<muteDto> {
        return this.muteService.giveMute(muteDto);
    }
}
