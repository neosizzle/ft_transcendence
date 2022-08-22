import React, { FunctionComponent } from "react";
import { useNavigate } from "react-router-dom";
import { Message } from "../../Chat/classes";
import { OUTGOING_JOIN_QUEUE } from "../../constants";
import { useAuth } from "../../context/authContext";

const INV_STR = "/invite/";

interface ChatCardProps {
  message: Message;
  floatRight?: boolean;
  system?: boolean;
}

const ChatCard: FunctionComponent<ChatCardProps> = ({
  message,
  floatRight,
  system,
}) => {
  const auth = useAuth();
  const isInvite = message.message.startsWith(INV_STR);
  const inviteDetails = message.message.split("/");
  const navigate = useNavigate();

  return (
    <div
      className={`py-5 ${
        system || isInvite
          ? "text-center"
          : floatRight
          ? "text-right"
          : "text-left"
      }`}
    >
      {" "}
      {isInvite ? (
        <span className="text-sm text-gray-400">
          {
            inviteDetails[3] !== auth?.user?.id.toString() ?
            "Invite sent" : 
            <button
            onClick={()=>{
                auth?.gameSocket?.emit(OUTGOING_JOIN_QUEUE, inviteDetails[2])
                navigate("/game")
            }}>
              you have been invited to a queue. click to join
            </button>
          }
        </span>
      ) : floatRight ? (
        message.message
      ) : system ? (
        <span className="text-sm text-gray-400">{message.message}</span>
      ) : (
        `${message.user?.username} : ${message.message}`
      )}
    </div>
  );
};

export default ChatCard;
