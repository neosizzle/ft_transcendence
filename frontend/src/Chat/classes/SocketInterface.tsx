import { io, Socket } from "socket.io-client";
import { TOKEN_KEY, WS_ROOT } from "../../constants";

const chatWsEndpoint = `${WS_ROOT}/chat`

// TODO: Type the ws responses
export class SocketInterface
{
	socket : Socket | null = null;

	constructor(onNewMessage : (data : any) => void)
	{
		//connect to ws server
		this.socket = io(chatWsEndpoint, {
			extraHeaders: {
			Authorization: `Bearer ${localStorage.getItem(TOKEN_KEY)}`,
			},
		});

		//attatch listeners
		this.socket.on('connection accepted', () => {
			this.socket?.emit('authHandshake')
			// join all rooms
		});


		this.socket.on('newMessage', (data)=>{
			onNewMessage(data)
		})
	}
	
	destroy()
	{
		this.socket?.off('connection accepted')
		this.socket?.disconnect();
	}

}