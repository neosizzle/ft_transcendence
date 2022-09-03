import { User } from "../../context/authContext";

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

export class roomPatchDto {
  roomName?: string;

  password?: string;

  ownerId?: number;

  isProtected?: boolean;
}

// incoming data received from server
export interface BaseWSResponse {
  userId: number | null;
  roomId: number;
  message: string;
  createdAt?: Date;
  updatedAt?: Date;
}

export interface Room {
  id: number;
  roomName?: string;
  ownerId: number;
  type: string;
  isProtected: boolean;
  createdAt: Date;
  updatedAt: Date;
}

export interface Member {
  id: number;
  userId: number;
  roomId: number;
  user: User;
  room: Room;
}

export interface Message {
  id: number;
  userId?: number;
  roomId: number;
  message: string;
  createdAt: Date;
  updatedAt: Date;
  room: Room;
  user?: User;
}

export interface Ban {
  id: number;
  userId: number;
  roomId: number;
  room : Room;
  user : User;
  expiresAt: Date;
}

export interface Admin {
  id: number;
  userId: number;
  roomId: number;
  user: User;
  room: Room;
}