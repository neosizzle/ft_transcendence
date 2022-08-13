import { BadRequestException, Injectable } from "@nestjs/common";
import { Chat, Prisma, RoomType } from "@prisma/client";
import { PrismaService } from "src/prisma/prisma.service";
import { ListQuery, validateListquery } from "src/utils";
import { MemberService } from "../member/member.service";
import { RoomService } from "../room/room.service";
import { chatDto } from "./chat.dto";
import generateChatPayload from "./chat.payload";

@Injectable()
export class ChatService {
  constructor(private readonly prismaService: PrismaService,
    private readonly memberService: MemberService,
    private readonly roomService: RoomService) { }

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
  // TODO: (After checking if user is in a room) Need to check if user is actually in RoomType DM or GC, if DM, check if blocked, else
  // if GC, check if user is muted.
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
    const theRoom = await this.prismaService.room.findFirst({
      where: {
        id: { equals: dto.roomId },
      }
    })
    if (theRoom.type == RoomType.DM) {
      // Then I would need to get the other userId in the room somehow and check if that
      // userId has blocked current userId sending the message
      // theRoom.data[0].members (doesn't work) idk how, think later. It's 2am bro
      // Can use members and find the member other than the one that is being parsed in from chat
      console.log("Hi BROOOOOO");
    }
    else if (theRoom.type == RoomType.GC) {
      const data3 = await this.prismaService.mute.findFirst({
        where: {
          roomId: { equals: dto.roomId },
          userId: { equals: dto.userId },
          expiresAt: { gt: new Date() },
        }
      })
      if (data3 != null)
        throw new BadRequestException("User is muted until " + data3.expiresAt.toISOString() + ".");
    }
    return this.prismaService.chat.create({
      include : {
        room : true,
        user : true
      },
      data: dto,
    });
  }
}
