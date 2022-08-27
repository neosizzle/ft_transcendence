import { User } from "../../context/authContext";

export interface Match {
	id            :number;
	playerId0     :number;
	playerId1     :number;
	winnerId      :number;
	playerScore0  :number;
	playerScore1  :number;
	isCustom     :boolean;
	player0       :User;
	player1       :User;
	winner        :User;
	createdAt :Date;
	updatedAt :Date;
}