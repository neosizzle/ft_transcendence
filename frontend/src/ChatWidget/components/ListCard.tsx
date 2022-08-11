import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { auth_net_get } from "../../utils";
import { Room } from "../types";
import CardLoader from "./CardLoader";
import GameInvBtn from "./GameInvBtn";

interface ListCardProps {
  room: Room;
  setActiveRoom : React.Dispatch<React.SetStateAction<Room | null>>;
}

const DEF_PIC = "/assets/default-pp.webp";
const memberEndpoint = `${API_ROOT}/members`;

const ListCard: FunctionComponent<ListCardProps> = ({ room , setActiveRoom}) => {
  const [loading, setLoading] = useState<boolean>(true); //loading state
  const [user, setUser] = useState<User | null>(null); // Opposing user if chat is a DM
  const [lastMsg, setLastMsg] = useState<string>(
    "User :  Some body once told me the world is gonna roll me"
  ); // lastest message for information section
  const navigate = useNavigate();
  const auth = useAuth();

  useEffect(() => {
    setLoading(true);
    if (room.type === "DM") {
      auth_net_get(
        `${memberEndpoint}?page=1&pageSize=2&filterOn=roomId&filterBy=${room.id}`
      ).then((data) => {
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        if (data.data[0].userId === auth?.user?.id) {
          setUser(data.data[1].user);
        } else {
          setUser(data.data[0].user);
        }
        setLoading(false);
      });
    }
    else
    {
      setUser(null);
      setLoading(false);
    }
  }, [room]);

  return loading ? (
    <CardLoader />
  ) : (
    <div
    onClick={(e)=>{
      setActiveRoom(room)
      e.stopPropagation()
    }}
    className="border-b-2 grid grid-cols-6 cursor-pointer">
      {/*Pic */}
      <div className="col-span-1">
        <img
          src={user ? (user.avatar as string | undefined) : DEF_PIC}
          className="h-full w-full rounded-full"
        />
      </div>

      {/* Info */}
      <div className="col-span-4 px-2">
        <div className="text-md lg:text-lg">
          {user ? user.username : room.roomName}
        </div>
        <div className="text-sm truncate">{lastMsg}</div>
      </div>

      {/* Action */}
      {user ? <GameInvBtn user={user} /> : null}
    </div>
  );
};

export default ListCard;
