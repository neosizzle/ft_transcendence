import { BadRequestException, Injectable } from "@nestjs/common";
import { Chat, Prisma } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListQuery, validateListquery } from "src/utils";
import { MemberService } from "../member/member.service";
import { MuteService } from "../mute/mute.service";
import { chatDto } from "./chat.dto";
import generateChatPayload from "./chat.payload";

@Injectable()
export class ChatService {
  constructor(private readonly prismaService: PrismaService,
    private readonly memberService: MemberService,
    private readonly muteService: MuteService) {}

  private sampleChat: Chat = {
    id: 0,
    userId: 0,
    roomId: 0,
    message: "",
    createdAt: new Date(),
    updatedAt: new Date(),
  };
  async getChat(query: ListQuery) {
    const keys = Object.keys(this.sampleChat).filter((e) => e);

    // validate query
    const listObj = validateListquery(query, keys);
    if (listObj.error) throw new BadRequestException(listObj.error);

    // generate and send prisma query
    const payload = generateChatPayload(listObj.data);
    const res = await this.prismaService.chat.findMany(payload);
    delete payload.skip;
    delete payload.take;
    delete payload.include;
    const total_elements = await this.prismaService.chat.count(
      <Prisma.ChatCountArgs>payload
    );
    return { data: res, total_elements };
  }
  async insertChat(dto: chatDto) {
    const query: ListQuery = {
      filterBy: `${dto.roomId},${dto.userId}`,
      filterOn: "roomId,userId",
      page: "1",
      pageSize: "1",
      operator: "AND",
    }
    const data = await this.memberService.getMembers(query);
    if (data.total_elements == 0)
      throw new BadRequestException("User is not in the room.");
    const data2 = await this.prismaService.mute.findFirst({
      where : {
        roomId : {equals : dto.roomId},
        userId : {equals : dto.userId},
        expiresAt : {gt : new Date()},
      }
    })
    if (data2 != null)
      throw new BadRequestException("User is muted until " + data2.expiresAt.toISOString() + ".");
    return this.prismaService.chat.create({
      data: dto,
    });
  }
}
