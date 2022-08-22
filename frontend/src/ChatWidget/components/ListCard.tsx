import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Room } from "../../Chat/classes";
import { API_ROOT } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChatWidget } from "../../context/chatWidgetContext";
import { auth_net_get } from "../../utils";
import CardLoader from "./CardLoader";
import GameInvBtn from "./GameInvBtn";

interface ListCardProps {
  room: Room;
  idx: number;
}

const DEF_PIC = "/assets/default-pp.webp";
const memberEndpoint = `${API_ROOT}/members`;
const INV_STR = "/invite/"

const ListCard: FunctionComponent<ListCardProps> = ({ room, idx }) => {
  const [loading, setLoading] = useState<boolean>(true); //loading state
  const [user, setUser] = useState<User | null>(null); // Opposing user if chat is a DM
  const navigate = useNavigate();
  const auth = useAuth();
  const widget = useChatWidget();

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
    } else {
      setUser(null);
      setLoading(false);
    }
  }, [room]);

  return loading ? (
    <CardLoader />
  ) : (
    <div
      onClick={(e) => {
        // set notification
        const notifyCpy = widget?.notify?.filter((e) => e !== room.id);
        if (notifyCpy?.length === 0) widget?.setNotify(null);
        else widget?.setNotify(notifyCpy as number[]);

        widget?.setCurrActiveRoom(room);
        e.stopPropagation();
      }}
      className="border-b-2 grid grid-cols-6 cursor-pointer"
    >
      {/*Pic */}
      <div className="col-span-1 relative">
        <img
          src={user ? (user.avatar as string | undefined) : DEF_PIC}
          className="h-full w-full"
        />

        {/* Notification indicator */}
        {widget?.notify?.includes(room.id) ? (
          <span className="top-0 left-7 absolute w-3.5 h-3.5 bg-green-400 border-2 dark:border-gray-800 rounded-full lg:left-1.5 lg:top-2.5"></span>
        ) : null}
      </div>

      {/* Info */}
      <div className="col-span-4 px-2">
        <div className="text-md lg:text-lg">
          {user ? user.username : room.roomName}
        </div>

        <div className="text-sm truncate">
          {widget?.lastMessages && widget.lastMessages[idx]
            ? widget.lastMessages[idx].user
              ? `${widget.lastMessages[idx].user?.username} : ${widget?.lastMessages[idx]?.message}`
              : widget?.lastMessages[idx]?.message.startsWith(INV_STR) ? "Game invite" : widget?.lastMessages[idx]?.message
            : null}
        </div>
      </div>

      {/* Action */}
      {user ? <GameInvBtn user={user} room={room} /> : null}
    </div>
  );
};

export default ListCard;
