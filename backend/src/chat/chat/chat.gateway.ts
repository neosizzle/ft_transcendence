import { BadRequestException, Logger, NotFoundException, NotImplementedException, UseGuards } from '@nestjs/common';
import { ConnectedSocket, MessageBody, OnGatewayConnection, OnGatewayDisconnect, SubscribeMessage, WebSocketGateway, WebSocketServer } from '@nestjs/websockets';
import { IsString } from 'class-validator';
import { Socket, Server, Namespace } from 'socket.io';
import { AuthGuard } from 'src/users/auth/guard';
import { BlocksService } from 'src/users/blocks/blocks.service';
// import { roomDto } from 'src/room/room.dto';

function getAllFuncs(toCheck) {
  const props = [];
  let obj = toCheck;
  do {
      props.push(...Object.getOwnPropertyNames(obj));
  } while (obj = Object.getPrototypeOf(obj));
  
  return props.sort().filter((e, i, arr) => { 
     if (e!=arr[i+1] && typeof toCheck[e] == 'function') return true;
  });
}

const GROUP_CHAT = 'gc'
const DIRECT_MESSAGE = 'dm'

const PROMOTE_ADMIN = 'promote'
const DEMOTE_ADMIN = 'demote'

class Clients {
  userId : string;
  socketId : string;
}

class CreateEventDto {
  type : string;
  userId : string;
  initialUsers : string[];
}

class LeaveEventDto {
  @IsString()
  roomId : string;
}

class JoinEventDto {
  @IsString()
  roomId : string;

  @IsString()
  password? : string;
}

class OwnerTransferDto {
  userId : string;
  roomId : string;
}

class ModifyAdminDto {
  userId : string;
  roomId : string;
  action : string;
}

class MessageEventDto {
  @IsString()
  roomId: string;

  @IsString()
  message : string;
}

class BanEventDto {
  @IsString()
  roomId : string;

  @IsString()
  baneeId : string;

  @IsString()
  duration : number;
}

class MuteEventDto {
  @IsString()
  roomId : string;

  @IsString()
  muteeId : string;

  @IsString()
  duration : number;
}

@UseGuards(AuthGuard)
@WebSocketGateway({namespace : "chat", cors : true})
export class ChatGateway implements OnGatewayConnection, OnGatewayDisconnect {

  // when used in a namespace, it returns a namespace instead.
  @WebSocketServer()
  // wsServer : Server;
  wsServer : Namespace;

  clients : Clients[] = [];
  logger : Logger = new Logger(ChatGateway.name);

  // TODO need chatservice and roomservice banservice , muteservice
  constructor(private block : BlocksService){}

  /**
   * New client connection
   * 
   * @param client Client socket
   * @param args 
   */
  handleConnection(client: Socket, ...args: any[]) {
    // return ack
    console.log("new incoming connection")
    client.emit("connection accepted", client.handshake)
  }

  /**
   * Handle client disconnection
   * @param client Client socket
   */
  handleDisconnect(client: Socket) {

    // remove from client list
    this.clients = this.clients.filter((e)=>client.id !== e.socketId)
    this.clients.forEach((_client)=> this.logger.log(`dc socketId ${_client.socketId}, userId ${ _client.userId}`))
  }

  /**
   * Handles auth handshake event
   * @param client CLient socket
   * @param payload 
   */
  @SubscribeMessage('authHandshake')
  handleAuthHandshake(@ConnectedSocket() client : Socket, @MessageBody() payload : string) {
    // add to client list
    this.clients.push({userId : client.handshake.auth.user.id, socketId : client.id})

    // add new socket to user rooms


    this.clients.forEach((_client)=> this.logger.log(`new client socketId ${_client.socketId}, userId ${ _client.userId}`))
  }

  /**
   * Handles create new dm / gc
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage('create')
  handleCreate(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
  // parse payload and assign it to dto
  const dto : CreateEventDto = JSON.parse(payload);

  // if its a dm, create room in db with initial user and no admin
  if (dto.type === DIRECT_MESSAGE) console.log("create dm room in db")

  // if its a gc, create room in db with admin as current user and no initial users (if got initial users, send system message to inform users)
  if (dto.type === GROUP_CHAT) console.log("Create gc room in db")

  // add curr user to room
  const roomId = `${dto.userId}-${dto.type}`
  client.join(roomId)

  // add initial users to room as well if they are online (connected to ws server)
  // console.log(getAllFuncs(this.wsServer))
  const namespace = this.wsServer;
  for (let userId of dto.initialUsers) {
    const client = this.clients.find((client) => userId.toString() === client.userId.toString())
    if (!client) continue ;
    const clientSocket = namespace.sockets.get(client.socketId)
    clientSocket.join(roomId)

    // inform initial users that group is created
    // emmit message from system 'you have been added herer by ${userId}'
    if (dto.type === GROUP_CHAT) clientSocket.emit('newMessage', {userId : null, roomId, message : `You have been added to room ${roomId}`})
  }
  

  client.emit("messageReceived", new NotImplementedException())
  }

  /**
   * Handles leave gc
   * @param client Client socket
   * @param payload Request payload
   */
   @SubscribeMessage('leave')
   handleLeave(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
    // parse payload and assign it to dto
    const dto : LeaveEventDto = JSON.parse(payload);

    // do nothing if room is a DM

    // leave room in db

    // emmit brodcast and notification to roomid that this user left
    client.leave(dto.roomId)
    this.wsServer.to(dto.roomId).emit('newMessage', {userId : null, roomId : dto.roomId, message : `${client.handshake.auth.user.username} has left`})
    client.emit("messageReceived", new NotImplementedException())
   }

  /**
   * Handles join dm / gc
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage('join')
  handleJoin(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
    // parse payload and assign it to dto
    const dto : JoinEventDto = JSON.parse(payload);

    // check if room exists, user has permission to join, password is correct
    // join room in db

    // join room in ws
    client.join(dto.roomId)

    // if its a gc, emmit brodcast and notification to roomid that this user joined
    // if (room.type == 'GC')
    //   this.wsServer.to(dto.roomId).emit('newMessage', {userId : null, roomId : dto.roomId, message : `${client.handshake.auth.user.username} has joined`})

    client.emit("messageReceived", new NotImplementedException())
  }

  /**
   * Handles gc ownertransfer
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage('ownertransfer')
  handleOwnerTransfer(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
    // parse payload and assign it to dto
    const dto : OwnerTransferDto = JSON.parse(payload);

    // check if room exists

    // if room is not Gc, return

    // modify room in db

    // emmit broadcast message and notification that admin had been tranferred
    this.wsServer.to(dto.roomId).emit('newMessage', {userId : null, roomId : dto.roomId, message : `${dto.userId} has become owner`})

    client.emit("messageReceived", new NotImplementedException())
  }

  /**
   * Handles gc modifyadmin
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage('modifyadmin')
  handleModifyAdmin(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
    // parse payload and assign it to dto
    const dto : ModifyAdminDto = JSON.parse(payload);

    // check if room exists

    // if room is not gc, return

    // modify room in db

    // emmit broadcast message and notification that admin had been tranferred
    if (dto.action === PROMOTE_ADMIN)
      this.wsServer.to(dto.roomId).emit('newMessage', {userId : null, roomId : dto.roomId, message : `${dto.userId} has became admin`})
    else
      this.wsServer.to(dto.roomId).emit('newMessage', {userId : null, roomId : dto.roomId, message : `${dto.userId} has been demoted`})

    client.emit("messageReceived", new NotImplementedException())
  }

  /**
   * Handles new message
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage('message')
  handleMessage(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
    // parse payload and assign it to dto
    const dto : MessageEventDto = JSON.parse(payload);

    // check if room id exists

    // determine if room is DM or GC.

    // if its a DM, check if user is blocked and send a BadReq if it is

    // if its a GC, check if user is muted and send a BadReq if it is

    // create new message in database

    // broadcast message into roomId
    this.wsServer.to(dto.roomId).emit('newMessage', {userId : client.handshake.auth.user.id, roomId : dto.roomId, message : dto.message})

    // broadcast notification event into roomid to notify all other users

    client.emit("messageReceived", new NotImplementedException())
  }

  /**
   * Handles ban 
   * @param client Client socket
   * @param payload Request payload
  */
  @SubscribeMessage('ban')
  handleBan(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
    // parse payload and assign it to dto
    const dto : BanEventDto = JSON.parse(payload);

    // check if room id exists

    // check if its a gc

    // check if user has privelleges

    // create new ban record in database

    // remove user from room in db

    // remove user from room in ws
    const baneeClient = this.clients.find((client) => dto.baneeId.toString() === client.userId.toString())
    if (!baneeClient) throw new NotFoundException("Banee not found");
    const baneeClientSocket = this.wsServer.sockets.get(baneeClient.socketId)
    baneeClientSocket.leave(dto.roomId)


    // broadcast system message into roomId
    this.wsServer.to(dto.roomId).emit('newMessage', {userId : null, roomId : dto.roomId, message :  `${dto.baneeId} got banned`})

    // broadcast notification event into roomid to notify all other users

    client.emit("messageReceived", new NotImplementedException())
  }

  /**
   * Handles mute
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage('mute')
  handleMute(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
    // parse payload and assign it to dto
    const dto : MuteEventDto = JSON.parse(payload);

    // check if room id exists

    // check if its a gc

    // check if user has privelleges

    // create new mute record in database

    // broadcast system message into roomId
    this.wsServer.to(dto.roomId).emit('newMessage', {userId : null, roomId : dto.roomId, message :  `${dto.muteeId} got muted`})

    // broadcast notification event into roomid to notify all other users

    client.emit("messageReceived", new NotImplementedException())
  }
}
