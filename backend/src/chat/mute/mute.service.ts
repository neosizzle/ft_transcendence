import { BadRequestException, Injectable } from "@nestjs/common";
import { Prisma, Mute } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListQuery, validateListquery } from "src/utils";
import { muteDto } from "./mute.dto";

@Injectable()

export class MuteService {
    constructor(private readonly prismaService: PrismaService) { }

    private sampleMute: Mute = {
        id: 0,
        userId: 0,
        roomId: 0,
        expiresAt: new Date(),
    };
    async getMute(query: ListQuery) {
        const keys = Object.keys(this.sampleMute).filter((e) => e);
        // validate query
        const listObj = validateListquery(query, keys);
        if (listObj.error) throw new BadRequestException(listObj.error);

        // generate and send prisma query
        const payload = generateRoomPayload(listObj.data);
        const res = await this.prismaService.room.findMany(payload);
        delete payload.skip;
        delete payload.take;
        delete payload.include;
        const total_elements = await this.prismaService.room.count(
            <Prisma.RoomCountArgs>payload
        );
        return { data: res, total_elements };
    }
    giveMute(dto: muteDto): Promise<muteDto> {
        //Note that "2022-08-02 doesn't work. Must be complete ISO8601 string, 2022-08-02T05:42:38.573Z"
        return this.prismaService.mute.create({
            data: dto,
        })
    }
}