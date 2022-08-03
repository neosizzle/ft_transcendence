import { RoomType } from "@prisma/client";
import {
  IsIn,
  IsNotEmpty,
  IsOptional,
  IsString,
  MinLength,
} from "class-validator";

export class roomDto {
  @IsOptional()
  @IsString()
  roomName: string;

  @IsNotEmpty()
  @IsString()
  @IsIn(Object.keys(RoomType))
  type: string;

  @IsString()
  @IsOptional()
  @MinLength(7)
  password: string;

  @IsString()
  @IsOptional()
  initialUsers: string;
}
