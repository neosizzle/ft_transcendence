import { BadRequestException, NotImplementedException, UseGuards } from '@nestjs/common';
import { ConnectedSocket, MessageBody, OnGatewayConnection, SubscribeMessage, WebSocketGateway } from '@nestjs/websockets';
import { IsString } from 'class-validator';
import { Socket } from 'socket.io';
import { AuthGuard } from 'src/auth/guard';
import { BlocksService } from 'src/blocks/blocks.service';

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
}

class MuteEventDto {
  @IsString()
  roomId : string;

  @IsString()
  muteeId : string;
}

@UseGuards(AuthGuard)
@WebSocketGateway({namespace : "chat", cors : true})
export class ChatGateway implements OnGatewayConnection {

  // TODO need chatservice and roomservice banservice , muteservice
  constructor(private block : BlocksService){}

  /**
   * New client connection
   * 
   * @param client Client socket
   * @param args 
   */
  handleConnection(client: Socket, ...args: any[]) {

    // join all available rooms
    client.join(["room1", "room2"])

    // return ack
    client.emit("connection accepted", client.handshake)
  }

  /**
   * Handles create new dm / gc
   * @param client Client socket
   * @param payload Request payload
   */
   @SubscribeMessage('create')
   handleCreate(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
     // parse payload and assign it to dto
 
     // if its a dm, create room in db with initial user and no admin

     // if its a gc, create room in db with admin as current user and no initial users (if got initial users, send system message to inform users)


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

     // leave room in db

     // emmit brodcast and notification to roomid that this user left
 
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

    // check if room exists, user has permission to join, password is correct
    // join room in db

    // if its a gc, emmit brodcast and notification to roomid that this user left

    client.emit("messageReceived", new NotImplementedException())
  }

  /**
   * Handles gc admintransfer
   * @param client Client socket
   * @param payload Request payload
   */
  @SubscribeMessage('admintransfer')
  handleAdminTransfer(@ConnectedSocket() client: Socket,  @MessageBody() payload: string) {
    // parse payload and assign it to dto

    // modify room in db

    // emmit broadcast message and notification that admin had been tranferred

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

    // check if room id exists

    // determine if room is DM or GC.

    // if its a DM, check if user is blocked and send a BadReq if it is

    // if its a GC, check if user is muted and send a BadReq if it is

    // create new message in database

    // broadcast message into roomId

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

    // check if room id exists

    // check if its a gc

    // check if user has privelleges

    // create new ban record in database

    // broadcast system message into roomId

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

    // check if room id exists

    // check if its a gc

    // check if user has privelleges

    // create new mute record in database

    // broadcast system message into roomId

    // broadcast notification event into roomid to notify all other users

    client.emit("messageReceived", new NotImplementedException())
  }
}
