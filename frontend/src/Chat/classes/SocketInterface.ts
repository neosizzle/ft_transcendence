import { io, Socket } from "socket.io-client";
import { TOKEN_KEY, WS_ROOT } from "../../constants";
import { BaseWSResponse, Message } from "./dto";

const chatWsEndpoint = `${WS_ROOT}/ws/chat`;

export class SocketInterface {
  socket: Socket | null = null;

  constructor(
    onNewMessage: (data : Message) => void,
    onException: (data: BaseWSResponse) => void,
    onOwnerChange: (data: BaseWSResponse) => void,
    onUserKicked: (data: BaseWSResponse) => void,
    onUserBanned: (data: BaseWSResponse) => void,
    onPromotion: (data: BaseWSResponse) => void,
    onDemotion: (data: BaseWSResponse) => void
  ) {
    //connect to ws server
    this.socket = io(chatWsEndpoint, {
      extraHeaders: {
        Authorization: `Bearer ${localStorage.getItem(TOKEN_KEY)}`,
      },
    });
    //attatch listeners
    this.socket.on("connection accepted", () => {
      this.socket?.emit("authHandshake");
      // join all rooms
    });

    // alert event (for dev)
    this.socket.on("alert", (data: string) => {
      console.log(data);
      alert(data);
    });

    // error event
    this.socket.on("exception", (data) => {
      onException(data);
    });

    // new message event
    this.socket.on("newMessage", (data) => {
      onNewMessage(data);
    });

    // owner transfer event
    this.socket.on("ownerChange", (data) => {
      onOwnerChange(data);
    });

    // kick event
    this.socket.on("userKicked", (data) => {
      onUserKicked(data);
    });

    // ban event
    this.socket.on("userBanned", (data) => {
      onUserBanned(data);
    });

    // promotion event
    this.socket.on("promotion", (data) => {
      onPromotion(data);
    });

    // ban event
    this.socket.on("demotion", (data) => {
      onDemotion(data);
    });
  }

  destroy() {
    this.socket?.off("connection accepted");
    this.socket?.disconnect();
  }
}
