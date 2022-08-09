// outgoing data sent to server
export interface roomDto {
  roomName?: string;
  type: string;
  password?: string;
  initialUsers?: string;
}

export interface memberDto {
  roomId: number;
  password?: string;
}

// incoming data received from server
export interface BaseWSResponse {
  userId: number | null;
  roomId: number;
  message: string;
}
