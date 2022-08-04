import { Injectable } from "@nestjs/common";
import { PrismaService } from "src/prisma/prisma.service";
import { banDto } from "./ban.dto";

@Injectable()

export class BanService {
    constructor(private readonly prismaService: PrismaService) {}

    giveBan(dto: banDto) : Promise<banDto> {
        return this.prismaService.ban.create({
            data: dto,
        })
    }
}