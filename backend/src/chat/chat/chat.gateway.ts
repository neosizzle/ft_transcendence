import {
  BadRequestException,
  Logger,
  NotFoundException,
  NotImplementedException,
  Param,
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
import { Admin, Member, Room } from "@prisma/client";
import { IsString } from "class-validator";
import { Socket, Server, Namespace } from "socket.io";
import { PrismaService } from "src/prisma/prisma.service";
import { AuthGuard } from "src/users/auth/guard";
import { BlocksService } from "src/users/blocks/blocks.service";
import { AdminService } from "../admin/admin.service";
import { AdminDto } from "../admin/dto";
import { banDto } from "../ban/ban.dto";
import { MemberDto } from "../member/dto";
import { MemberService } from "../member/member.service";
import { roomDto, roomPatchDto } from "../room/dto";
import { RoomService } from "../room/room.service";
import { chatDto } from "./chat.dto";
// import { roomDto } from 'src/room/room.dto';

function getAllFuncs(toCheck) {
  const props = [];
  let obj = toCheck;
  do {
    props.push(...Object.getOwnPropertyNames(obj));
  } while ((obj = Object.getPrototypeOf(obj)));

  return props.sort().filter((e, i, arr) => {
    if (e != arr[i + 1] && typeof toCheck[e] == "function") return true;
  });
}

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

  // TODO need chatservice and roomservice banservice , muteservice
  constructor(
    private room: RoomService,
    private prisma: PrismaService,
    private member: MemberService,
    private admin: AdminService
  ) {}

  /**
   * New client connection
   *
   * @param client Client socket
   * @param args
   */
  handleConnection(client: Socket) {
    // return ack
    console.log("new incoming connection");
    client.emit("connection accepted", client.handshake);
  }

  /**
   * Handle client disconnection
   * @param client Client socket
   */
  handleDisconnect(client: Socket) {
    // remove from client list
    this.clients = this.clients.filter((e) => client.id !== e.socketId);

    this.logger.log("DC CLIENT - current clients :");
    this.clients.forEach((_client) =>
      this.logger.log(`socketId ${_client.socketId}, userId ${_client.userId}`)
    );
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

    this.logger.log("NEW CLIENT CONNETED - current clients :");
    this.clients.forEach((_client) =>
      this.logger.log(`socketId ${_client.socketId}, userId ${_client.userId}`)
    );
  }

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
    const roomId = roomCreateRes.id.toString();
    client.join(roomId);

    // add initial users to room as well if they are online (connected to ws server)
    // console.log(getAllFuncs(this.wsServer))
    const namespace = this.wsServer;
    for (const userId of dto.initialUsers.split(",")) {
      const client = this.clients.find(
        (client) => userId.toString() === client.userId.toString()
      );
      if (!client) continue;
      const clientSocket = namespace.sockets.get(client.socketId);
      clientSocket.join(roomId);

      // inform initial users that group is created
      // emmit message from system 'you have been added herer by ${userId}'
      if (roomCreateRes.type === "GC") {
        clientSocket.emit("alert", `You have been added to room ${roomId}`);
        // clientSocket.emit("newMessage", {
        //   userId: null,
        //   roomId,
        //   message: `You have been added to room ${roomId}`,
        // });
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
    client.leave(res.roomId.toString());
    this.wsServer
      .to(res.roomId.toString())
      .emit("alert", `${client.handshake.auth.user.username} has left`);
    // this.wsServer.to(id).emit("newMessage", {
    //   userId: null,
    //   roomId: id,
    //   message: `${client.handshake.auth.user.username} has left`,
    // });
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
    client.join(member.id.toString());

    // if its a gc, emmit brodcast and notification to roomid that this user joined
    const room = await this.prisma.room.findUnique({
      where: { id: member.roomId },
    });

    if (room.type == "GC")
      this.wsServer
        .to(room.id.toString())
        .emit("alert", `${client.handshake.auth.user.username} has joined`);

    // if (room.type == "GC")
    //   this.wsServer.to(room.id.toString()).emit('newMessage', {userId : null, roomId : room.id, message : `${client.handshake.auth.user.username} has joined`})
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
  handleMessage(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // parse payload and assign it to dto
    const dto: chatDto = JSON.parse(payload);

    // check if room id exists

    // determine if room is DM or GC.

    // if its a DM, check if user is blocked and send a BadReq if it is

    // if its a GC, check if user is muted and send a BadReq if it is

    // create new message in database

    // broadcast message into roomId
    this.wsServer.to(dto.roomId.toString()).emit("newMessage", {
      userId: client.handshake.auth.user.id,
      roomId: dto.roomId,
      message: dto.message,
    });

    // broadcast notification event into roomid to notify all other users

    client.emit("messageReceived", new NotImplementedException());
  }

  /**
   * Handles ban
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage("ban")
  handleBan(@ConnectedSocket() client: Socket, @MessageBody() payload: string) {
    // parse payload and assign it to dto
    const dto: banDto = JSON.parse(payload);

    // check if room id exists

    // check if its a gc

    // check if user has privelleges

    // create new ban record in database

    // ban user in db

    // remove user from room in ws
    const baneeClient = this.clients.find(
      (client) => dto.userId.toString() === client.userId.toString()
    );
    if (baneeClient)
    {
      const baneeClientSocket = this.wsServer.sockets.get(baneeClient.socketId);
      baneeClientSocket.leave(dto.roomId.toString());
    }
    
    // broadcast ban event into roomId
    this.wsServer.to(dto.roomId.toString()).emit("userBanned", {
      userId: dto.userId,
      roomId: dto.roomId,
      message: `${dto.userId} got banned`,
    });

    // broadcast notification event into roomid to notify all other users

    client.emit("messageReceived", new NotImplementedException());
  }

}
