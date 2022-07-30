import { BadRequestException, UseGuards } from '@nestjs/common';
import { ConnectedSocket, MessageBody, OnGatewayConnection, SubscribeMessage, WebSocketGateway } from '@nestjs/websockets';
import { IsString } from 'class-validator';
import { Socket } from 'socket.io';
import { AuthGuard } from 'src/auth/guard';
import { BlocksService } from 'src/blocks/blocks.service';

class MessageEventDto {
  @IsString()
  room_id : string;

  @IsString()
  password : string;

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
   * 
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

    // broadcast notification event to notify all other users

    client.emit("messageReceived", new BadRequestException())
  }
}
