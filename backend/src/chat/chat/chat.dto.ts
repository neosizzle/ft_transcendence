import { IsNotEmpty, IsNumber, IsString } from "class-validator";

export class chatDto {
  @IsNotEmpty()
  @IsNumber()
  userId: number;

  @IsNotEmpty()
  @IsNumber()
  roomId: number;

  @IsNotEmpty()
  @IsString()
  message: string;
}

export class GameinvDto {
  @IsNotEmpty()
  @IsNumber()
  userId: number;

  @IsNotEmpty()
  @IsNumber()
  roomId: number;
}

// ws data sent to client
export class BaseWSResponse {
  userId: number | null;
  roomId: number;
  message: string;
}
