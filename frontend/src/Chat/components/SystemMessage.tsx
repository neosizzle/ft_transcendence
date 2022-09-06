import React, { FunctionComponent } from "react";
import { useNavigate } from "react-router-dom";
import { OUTGOING_JOIN_QUEUE } from "../../constants";
import { useAuth } from "../../context/authContext";
import { Message } from "../classes";

const INV_STR = "/invite/";

interface SystemMessageProps {
  message: Message;
}

const SystemMessage: FunctionComponent<SystemMessageProps> = ({ message }) => {
  const auth = useAuth();
  const navigate = useNavigate();
  const isInvite = message.message.startsWith(INV_STR);
  const inviteDetails = message.message.split("/");

  return (
    <div className="text-center">
      {isInvite ? (
        <span className="text-sm text-gray-400">
          {inviteDetails[3] !== auth?.user?.id.toString() ? (
            "Invite sent"
          ) : (
            <button
              onClick={() => {
                auth?.gameSocket?.emit(OUTGOING_JOIN_QUEUE, inviteDetails[2]);
                navigate("/game");
              }}
            >
              you have been invited to a queue. click to join
            </button>
          )}
        </span>
      ) : (
        <span className="text-sm text-gray-400">{message.message}</span>
      )}
    </div>
  );
};

export default SystemMessage;
