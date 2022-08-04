import { Body, Controller, Get, Post } from '@nestjs/common';
import { Ban } from '@prisma/client';
import { PrismaService } from 'src/prisma/prisma.service';
import { banDto } from './ban.dto';
import { BanService } from './ban.service';

@Controller('ban')
export class BanController {
    constructor(
        private readonly prismaService: PrismaService,
        private readonly banService: BanService
    ) { }

    @Get()
    getBan(): Promise<Ban[]> {
        return this.prismaService.ban.findMany();
    }
    @Post()
    create(@Body() banDto: banDto): Promise<banDto> {
        return this.banService.giveBan(banDto);
    }
}
