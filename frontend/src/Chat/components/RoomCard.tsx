import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { auth_net_get } from "../../utils";
import { Member, Room } from "../classes";

const memberEndpoint = `${API_ROOT}/members`;

interface RoomCardProps {
  room: Room;
}

const RoomCard: FunctionComponent<RoomCardProps> = ({ room }) => {
  const [otherDmUser, setOtherDmUser] = useState<User | null>(null);
  const navigate = useNavigate();
  const chat = useChat();
  const auth = useAuth();

  // get other dm user if room is a dm
  useEffect(() => {
    if (room.type === "DM") {
      auth_net_get(
        `${memberEndpoint}?page=1&pageSize=2&filterOn=roomId&filterBy=${room.id}`
      ).then((data) => {
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        const members: Member[] = data.data;
        if (members[0].userId === auth?.user?.id)
          setOtherDmUser(members[1].user);
        else if (members[1].userId === auth?.user?.id)
          setOtherDmUser(members[0].user);
      });
    }
  }, []);

  return (
    <div
      className="text-xl border-2 cursor-pointer"
      onClick={() => {
        chat?.setActiveRoom(room);
      }}
      key={room.id}
    >
      {
        room.type === "DM" ? (
          <img src={otherDmUser?.avatar as string} width="100" height="100" />
        ) : (
          <img src="/assets/default-gc-harambe.png" width="100" height="100" />
        ) // Can't pass in value to src for some reason?
      }
      {
        room.type === "DM" ? otherDmUser?.username : room.roomName // But how to get specific room's users
      }
    </div>
  );
};

export default RoomCard;
