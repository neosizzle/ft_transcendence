import { User } from "../../context/authContext";

export interface Room {
	id : number;
	roomName? : string;
	ownerId : number;
	type : string;
	isProtected : boolean;
	createdAt : Date;
	updatedAt : Date;	
}

export interface Member {
	id : number;
	userId : number;
	roomId : number;
	user : User;
	room : Room;
}