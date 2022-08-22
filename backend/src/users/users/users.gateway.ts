import { BadRequestException, Logger, UseGuards } from "@nestjs/common";
import {
  ConnectedSocket,
  MessageBody,
  OnGatewayConnection,
  OnGatewayDisconnect,
  SubscribeMessage,
  WebSocketGateway,
  WebSocketServer,
} from "@nestjs/websockets";
import { Socket, Namespace } from "socket.io";
import { PrismaService } from "src/prisma/prisma.service";
import { AuthGuard } from "../auth/guard";
import { UserPatchDto } from "./dto";

class Clients {
  userId: string;
  socketId: string;
}

@UseGuards(AuthGuard)
@WebSocketGateway({ namespace: "ws/users", cors: true })
export class UsersGateway implements OnGatewayConnection, OnGatewayDisconnect {
  // when used in a namespace, it returns a namespace instead.
  @WebSocketServer()
  // wsServer : Server;
  wsServer: Namespace;

  // connected clients and user ids
  clients: Clients[] = [];
  logger: Logger = new Logger(UsersGateway.name);

  constructor(private prisma: PrismaService) {}

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

  /**
   * Handles user status update
   * @param client connected client
   * @param payload user status update
   * @returns
   */
  @SubscribeMessage("updateUserStatus")
  async handleMessage(
    @ConnectedSocket() client: Socket,
    @MessageBody() payload: string
  ) {
    // parse payload and assign it to dto
    const dto: UserPatchDto = JSON.parse(payload);

    if (!dto.status) {
      client.emit("exception", new BadRequestException("No status in body"));
      return;
    }

    // get user
    const user = client.handshake.auth.user;

    // update user in db
    try {
      const res = await this.prisma.user.update({
        where: { id: user.id },
        data: { status: dto.status },
      });

      // emit this update to all connected clients
      this.wsServer.emit("statusUpdate", res)
    } catch (error) {
      client.emit("exception", error);
      return;
    }
  }
}
