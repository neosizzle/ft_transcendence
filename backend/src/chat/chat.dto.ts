import {IsDateString, IsNotEmpty, IsNumber, IsString } from "class-validator";

export class chatDto {
    id: number;

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