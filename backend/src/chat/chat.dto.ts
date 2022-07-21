import {IsDateString, IsNotEmpty, IsNumber, IsString } from "class-validator";

export class chatDto {
    id: number;

    @IsNotEmpty()
    @IsNumber()
    UserID: number;

    @IsNotEmpty()
    @IsDateString()
    timestamp: Date;

    @IsNotEmpty()
    @IsString()
    message: string;
}