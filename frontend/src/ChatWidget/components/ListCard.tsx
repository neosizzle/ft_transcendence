import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { useAuth } from "../../context/authContext";
import { auth_net_get } from "../../utils";
import { Room } from "../types";
import CardLoader from "./CardLoader";

interface ListCardProps {
  room: Room;
}

const DEF_PIC = "/assets/default-pp.webp";
const memberEndpoint = `${API_ROOT}/members`;
const ListCard: FunctionComponent<ListCardProps> = ({ room }) => {
  const [loading, setLoading] = useState<boolean>(true); //loading state
  const [picture, setPicture] = useState<string>(DEF_PIC); // chat picture
  const [title, setTitle] = useState<string>(""); // title for information section
  const [lastMsg, setLastMsg] = useState<string>(""); // lastest message for information section
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
          setPicture(data.data[1].user.avatar);
          setTitle(data.data[1].user.username);
        } else {
          setPicture(data.data[0].user.avatar);
          setTitle(data.data[0].user.username);
        }
      });
    } else {
      setPicture(DEF_PIC);
      setTitle(room.roomName || "undefined");
    }
    setLoading(false);
  }, [room]);

  return loading ? (
    <CardLoader />
  ) : (
    <div className="border-b-2 grid grid-cols-6">
      {/* Pic */}
      <div className="col-span-1">
        <img src={picture || DEF_PIC} className="h-full w-full rounded-full" />
      </div>

      {/* Info */}
      <div className="col-span-4 px-2">
        <div className="text-md lg:text-lg">
          {title} {room.id}
        </div>
      </div>

      {/* Action */}
      <div className="col-span-1">Action</div>
    </div>
  );
};

export default ListCard;
