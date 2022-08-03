import { Injectable } from "@nestjs/common";
import { PrismaService } from "src/prisma/prisma.service";
import { muteDto } from "./mute.dto";

@Injectable()

export class MuteService {
    constructor(private readonly prismaService: PrismaService) {}

    giveMute(dto: muteDto): Promise<muteDto> {
        //Note that "2022-08-02 doesn't work. Must be complete ISO8601 string, 2022-08-02T05:42:38.573Z"
        return this.prismaService.mute.create({
            data: dto,
        })
    }
}