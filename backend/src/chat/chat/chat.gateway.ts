import {
  BadRequestException,
  InternalServerErrorException,
  Logger,
  UseGuards,
} from "@nestjs/common";
import {
  ConnectedSocket,
  MessageBody,
  OnGatewayConnection,
  OnGatewayDisconnect,
  SubscribeMessage,
  WebSocketGateway,
  WebSocketServer,
} from "@nestjs/websockets";
import { Admin, Ban, Chat, Member, Room } from "@prisma/client";
import { Socket, Namespace } from "socket.io";
import { PrismaService } from "src/prisma/prisma.service";
import { AuthGuard } from "src/users/auth/guard";
import { AdminService } from "../admin/admin.service";
import { AdminDto } from "../admin/dto";
import { banDto } from "../ban/ban.dto";
import { BanService } from "../ban/ban.service";
import { MemberDto } from "../member/dto";
import { MemberService } from "../member/member.service";
import { roomDto, roomPatchDto } from "../room/dto";
import { RoomService } from "../room/room.service";
import { chatDto, GameinvDto, SysMsg } from "./chat.dto";
import { ChatService } from "./chat.service";

const INV_STRING = "/invite/";
class Clients {
  userId: string;
  socketId: string;
}

@UseGuards(AuthGuard)
@WebSocketGateway({ namespace: "ws/chat", cors: true })
export class ChatGateway implements OnGatewayConnection, OnGatewayDisconnect {
  // when used in a namespace, it returns a namespace instead.
  @WebSocketServer()
  // wsServer : Server;
  wsServer: Namespace;

  clients: Clients[] = [];
  logger: Logger = new Logger(ChatGateway.name);

  constructor(
    private room: RoomService,
    private prisma: PrismaService,
    private member: MemberService,
    private admin: AdminService,
    private ban: BanService,
    private chat: ChatService
  ) {}

  /**
   * New client connection
   *
   * @param client Client socket
   * @param args
   */
  handleConnection(client: Socket) {
    // return ack
    client.emit("connection accepted", client.handshake);
  }

  /**
   * Handle client disconnection
   * @param client Client socket
   */
  handleDisconnect(client: Socket) {
    // remove from client list
    this.clients = this.clients.filter((e) => client.id !== e.socketId);
  }

  /**
   * Handles auth handshake event
   * @param client CLient socket
   * @param payload
   */
  @SubscribeMessage("authHandshake")
  async handleAuthHandshake(@ConnectedSocket() client: Socket) {
    // add to client list
    this.clients.push({
      userId: client.handshake.auth.user.id,
      socketId: client.id,
    });

    // add new socket to user rooms
    const members = await this.prisma.member.findMany({
      where: { userId: client.handshake.auth.user.id },
    });
    members.forEach((member) => {
      client.join(member.roomId.toString());
    });
  }

  // TODO work with edi for owner transfer and admin promotion/demotion
  /**
   * Handles create new dm / gc
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("create")
  async handleCreate(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // parse payload and assign it to dto
    const dto: roomDto = JSON.parse(payload);

    // add room in db
    let roomCreateRes: Room;
    try {
      roomCreateRes = await this.room.addRoom(client.handshake.auth.user, dto);
    } catch (error) {
      client.emit("exception", error);
      return;
    }

    // add curr user to room
    const user = client.handshake.auth.user;
    const roomId = roomCreateRes.id.toString();
    client.join(roomId);

    // add initial users to room as well if they are online (connected to ws server)
    const namespace = this.wsServer;
    let initUsers: string[] = [];
    if (dto.initialUsers)
      initUsers = dto.initialUsers.split(",");
    if (!initUsers.includes(user.id.toString()))
      initUsers.push(user.id.toString());
    for (const userId of initUsers) {
      const client = this.clients.find(
        (client) => userId.toString() === client.userId.toString()
      );
      if (!client) continue;
      const clientSocket = namespace.sockets.get(client.socketId);
      clientSocket.join(roomId);

      // inform initial users that group is created
      if (roomCreateRes.type === "GC") {
        const sysMsg: SysMsg = {
          id: -1,
          roomId: parseInt(roomId, 10),
          room: roomCreateRes,
          userId: null,
          message: `You have been invited to room ${roomId}`,
          createdAt: new Date(),
          updatedAt: new Date(),
        };
        console.log("emmiting inituser message to ", clientSocket.id)
        clientSocket.emit("newMessage", sysMsg);
      }
    }
  }

  /**
   * Handles leave gc
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("leave")
  async handleLeave(
    @ConnectedSocket() client: Socket,
    @MessageBody() memberId: string
  ) {
    // remove member in db
    let res: Member;
    try {
      res = await this.member.removeMember(
        client.handshake.auth.user,
        parseInt(memberId, 10)
      );
    } catch (error) {
      client.emit("exception", error);
      return;
    }

    // emmit brodcast and notification to roomid that this user left
    const sysMsg: SysMsg = {
      id: -1,
      roomId: res.roomId,
      room: await this.prisma.room.findUnique({ where: { id: res.roomId } }),
      userId: null,
      message: `${client.handshake.auth.user.username} has left`,
      createdAt: new Date(),
      updatedAt: new Date(),
    };
    client.leave(res.roomId.toString());
    this.wsServer.to(res.roomId.toString()).emit("newMessage", sysMsg);
  }

  /**
   * Handles join dm / gc
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("join")
  async handleJoin(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // parse payload and assign it to dto
    const dto: MemberDto = JSON.parse(payload);

    // check if room exists, user has permission to join, password is correct
    // join room in db
    let member: Member;
    try {
      member = await this.member.addMember(client.handshake.auth.user, dto);
    } catch (error) {
      client.emit("exception", error);
      return;
    }

    // join room in ws
    client.join(member.roomId.toString());

    // if its a gc, emmit brodcast and notification to roomid that this user joined
    const room = await this.prisma.room.findUnique({
      where: { id: member.roomId },
    });

    if (room.type === "GC") {
      const sysMsg: SysMsg = {
        id: -1,
        roomId: room.id,
        room: room,
        userId: null,
        message: `${client.handshake.auth.user.username} has joined`,
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      this.wsServer.to(room.id.toString()).emit("newMessage", sysMsg);
    }
  }

  /**
   * Handles gc ownertransfer
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("ownertransfer")
  async handleOwnerTransfer(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // parse payload and assign it to dto
    const payloadParsed = JSON.parse(payload);
    const dto: roomPatchDto = payloadParsed.dto;
    const roomId: number = payloadParsed.roomId;

    if (dto.roomName || dto.isProtected || dto.password || !dto.ownerId) {
      client.emit(
        "exception",
        new BadRequestException(
          "Only owner transfer allowed through this endpoint"
        )
      );
      return;
    }

    // modify room in db
    let patchRes: Room;
    try {
      patchRes = await this.room.modifyRoom(
        client.handshake.auth.user,
        roomId.toString(),
        dto
      );
    } catch (error) {
      client.emit("exception", error);
      return;
    }

    // emmit broadcast message and notification that admin had been tranferred
    this.wsServer.to(patchRes.id.toString()).emit("ownerChange", {
      userId: patchRes.ownerId,
      roomId: patchRes.id,
      message: `${patchRes.ownerId} has become owner`,
      createdAt: patchRes.createdAt,
      updatedAt: patchRes.updatedAt,
    });
  }

  /**
   * Handles gc promoteadmin
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("promoteAdmin")
  async handlePromoteAdmin(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // parse payload and assign it to dto
    const dto: AdminDto = JSON.parse(payload);

    // add admin in db
    let adminRes: Admin;
    try {
      adminRes = await this.admin.addAdmin(client.handshake.auth.user, dto);
    } catch (error) {
      client.emit("exception", error);
      return;
    }

    // emmit broadcast message and notification that admin had been promoted
    this.wsServer.to(dto.roomId.toString()).emit("promotion", {
      userId: adminRes.userId,
      roomId: adminRes.roomId,
      message: `${adminRes.userId} has became admin`,
    });
  }

  /**
   * Handles gc promoteadmin
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("demoteAdmin")
  async handleDemoteAdmin(
    @ConnectedSocket() client: Socket,
    @MessageBody() adminId: string
  ) {
    // delete admin in db
    let deleteRes: Admin;
    try {
      deleteRes = await this.admin.removeAdmin(
        client.handshake.auth.user,
        parseInt(adminId, 10)
      );
    } catch (error) {
      client.emit("exception", error);
      return;
    }
    // emmit broadcast message and notification that admin had been demoted
    this.wsServer.to(deleteRes.roomId.toString()).emit("demotion", {
      userId: deleteRes.userId,
      roomId: deleteRes.roomId,
      message: `${deleteRes.userId} has became admin`,
    });
  }
  /**
   * Handles new message
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("message")
  async handleMessage(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // parse payload and assign it to dto
    const dto: chatDto = JSON.parse(payload);

    // add chat in db
    let chatRes: Chat;
    try {
      chatRes = await this.chat.insertChat(dto);
    } catch (error) {
      if (!error.message) throw new InternalServerErrorException(error);
      else client.emit("exception", error);
      return;
    }

    // get room members
    const members = await this.prisma.member.findMany({
      where: {
        roomId: chatRes.roomId,
      },
    });

    // get members who isnt in ws room
    const clientsInRoom = this.wsServer.adapter.rooms.get(
      chatRes.roomId.toString()
    );
    members.forEach((e) => {
      const connectedClient = this.clients.find(
        (client) => client.userId.toString() === e.userId.toString()
      );
      // if user is connected to ws server and not join ws room
      if (connectedClient && !clientsInRoom.has(e.userId.toString())) {
        this.wsServer.sockets
          .get(connectedClient.socketId)
          .join(chatRes.roomId.toString());
      }
    });

    // broadcast message into roomId
    this.wsServer.to(dto.roomId.toString()).emit("newMessage", chatRes);
  }

  /**
   * Handles kick
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("kick")
  async handleKick(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // parse payload and assign it to dto
    const payloadParsed = JSON.parse(payload);
    const memberId = payloadParsed.memberId;

    // remove member in db
    let kickRes: Member;
    try {
      kickRes = await this.member.removeMember(
        client.handshake.auth.user,
        memberId
      );
    } catch (error) {
      client.emit("exception", error);
      return;
    }
    // remove user from room in ws
    const userClient = this.clients.find(
      (client) => kickRes.userId.toString() === client.userId.toString()
    );
    if (userClient) {
      const userClientSocket = this.wsServer.sockets.get(userClient.socketId);
      userClientSocket.leave(kickRes.roomId.toString());
    }

    // broadcast ban event into roomId
    this.wsServer.to(kickRes.roomId.toString()).emit("userKicked", kickRes);
  }

  /**
   * Handles ban
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("ban")
  async handleBan(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // parse payload and assign it to dto
    const dto: banDto = JSON.parse(payload);

    // ban user in db
    let banRes: Ban;
    try {
      banRes = await this.ban.giveBan(client.handshake.auth.user, dto);
    } catch (error) {
      client.emit("exception", error);
      return;
    }
    // remove user from room in ws
    const baneeClient = this.clients.find(
      (client) => dto.userId.toString() === client.userId.toString()
    );
    if (baneeClient) {
      const baneeClientSocket = this.wsServer.sockets.get(baneeClient.socketId);
      baneeClientSocket.leave(dto.roomId.toString());
    }

    // broadcast ban event into roomId
    this.wsServer.to(dto.roomId.toString()).emit("userBanned", banRes);
  }

  /**
   * Handles ban
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("invite")
  async handleInvite(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // extract information
    const dto: GameinvDto = JSON.parse(payload);
    const currUser = client.handshake.auth.user;
    const userIdToInv = dto.userId;
    const roomIdToInv = dto.roomId;

    // check if user is inviting self
    if (userIdToInv === currUser.id) {
      client.emit(
        "exception",
        new BadRequestException("Cant invite self to game")
      );
      return;
    }

    // check if both current user and user to invite is in the same room as dto.roomid
    const member = await this.prisma.member.findFirst({
      where: {
        userId: userIdToInv,
        roomId: roomIdToInv,
      },
    });
    if (!member) {
      client.emit("exception", new BadRequestException("Invalid invitation"));
      return;
    }

    // get room
    const room = await this.prisma.room.findUnique({
      where: { id: member.roomId },
    });

    // check if room is dm and user is not blocked
    if (room.type === "GC") {
      client.emit("exception", new BadRequestException("Cant invite in GC"));
      return;
    }

    const blocked = await this.prisma.block.findFirst({
      where: {
        OR: [
          {
            blockeeId: userIdToInv,
            blockerId: currUser.id,
          },
          {
            blockeeId: currUser.id,
            blockerId: userIdToInv,
          },
        ],
      },
    });
    if (blocked) {
      client.emit("exception", new BadRequestException("User blocked"));
      return;
    }

    // add message to chat
    let msgRes: Chat;
    try {
      msgRes = await this.prisma.chat.create({
        data: {
          userId: null,
          roomId: room.id,
          message: `${INV_STRING}${dto.queuePosition}/${userIdToInv}`,
        },
      });
    } catch (error) {
      client.emit("exception", new InternalServerErrorException(error.message));
      return;
    }

    // emit message
    this.wsServer.to(room.id.toString()).emit("newMessage", msgRes);
  }
}
